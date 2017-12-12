package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.core.fileProcessingPhase.{
  RowsProcessingPhase,
  ValidateAndMatchRows,
  ValidateAndMatchTwoFiles
}
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  UnrecoverableError
}
import com.scalableQuality.quick.mantle.parsing.errorMessages.GroupRowsByRowDescriptionErrorMessages
import com.scalableQuality.quick.surface.commandLineOptions.QuickState

import scala.annotation.tailrec
import scala.xml.{Elem, MetaData}
import scala.collection.mutable

class GroupRowsByRowDescription(
    val listOfRowIdentifier: List[RowToRowDescriptionMatcher],
    leftFileLabel: Option[String],
    rightFileLabel: Option[String],
    quickState: QuickState
) {
  def validateAndMatchTheseTwoFiles(
      leftFileRows: => List[RawRow],
      rightFileRows: => List[RawRow]
  ): ValidateAndMatchTwoFiles = {
    val groupLeftFileRowsByRowDescription =
      groupRowsByRowDescription(leftFileRows, listOfRowIdentifier)
    val groupRightFileRowsByRowDescription =
      groupRowsByRowDescription(rightFileRows, listOfRowIdentifier)
    val matchedGroups = matchGroupsRowsByRowDescription(
      groupLeftFileRowsByRowDescription,
      groupRightFileRowsByRowDescription)
    val validationAndMatchingProcesses: List[RowsProcessingPhase] =
      matchedGroups.map { rowsProcessingPhaseParameters =>
        quickState.rowsProcessingPhase(
          rowsProcessingPhaseParameters._1,
          rowsProcessingPhaseParameters._2,
          rowsProcessingPhaseParameters._3,
          this.leftFileLabel,
          this.rightFileLabel
        )
      }
    ValidateAndMatchTwoFiles(validationAndMatchingProcesses)
  }

  private def matchGroupsRowsByRowDescription(
      leftFileRowGroups: List[(OrderedRowDescription, List[RawRow])],
      rightFileRowGroups: List[(OrderedRowDescription, List[RawRow])]
  ): List[(OrderedRowDescription, List[RawRow], List[RawRow])] = {
    @tailrec def loop(
        leftFileRowGroups: List[(OrderedRowDescription, List[RawRow])],
        rightFileRowGroups: List[(OrderedRowDescription, List[RawRow])],
        accumulator: List[(OrderedRowDescription, List[RawRow], List[RawRow])]
    ): List[(OrderedRowDescription, List[RawRow], List[RawRow])] =
      leftFileRowGroups match {
        case Nil =>
          val remainingRightFileRowGroups =
            rightFileRowGroups.map(rowGroup => (rowGroup._1, Nil, rowGroup._2))
          remainingRightFileRowGroups ::: accumulator

        case (orderedRowDescription, leftFileRows) :: restOfLeftFileRowGroups =>
          val (matchingRightGroups, restOfRightFileRowGroups) =
            rightFileRowGroups.partition(_._1 == orderedRowDescription)
          val groupedByRowDescription = matchingRightGroups match {
            case Nil =>
              (orderedRowDescription, leftFileRows, Nil)

            case (_, rightFileRows) :: _ =>
              (orderedRowDescription, leftFileRows, rightFileRows)
          }
          loop(
            restOfLeftFileRowGroups,
            restOfRightFileRowGroups,
            groupedByRowDescription :: accumulator
          )
      }

    loop(leftFileRowGroups, rightFileRowGroups, Nil)
  }

  def groupRowsByRowDescription(
      rows: List[RawRow],
      rowIdentifiers: List[RowToRowDescriptionMatcher]
  ): List[(OrderedRowDescription, List[RawRow])] = {

    type RowDescriptionToRowsHashMap = mutable.HashMap[
      OrderedRowDescription,
      mutable.Set[RawRow]] with mutable.MultiMap[OrderedRowDescription, RawRow]

    @tailrec def loop(
        rows: List[RawRow],
        rowIdentifiers: List[RowToRowDescriptionMatcher],
        rowDescriptionToRowsHashMap: RowDescriptionToRowsHashMap
    ): List[(OrderedRowDescription, List[RawRow])] = rows match {
      case Nil =>
        val rowDescriptionAndRows = for {
          orderedRowAndItsRows <- rowDescriptionToRowsHashMap
        } yield (orderedRowAndItsRows._1, orderedRowAndItsRows._2.toList)
        rowDescriptionAndRows.toList

      case row :: restOfRows =>
        val orderedRowDescriptionOpt = rowIdentifiers.collectFirst {
          case rowIdentifier if rowIdentifier.canIdentify(row) =>
            rowIdentifier.orderedRowDescription
        }
        orderedRowDescriptionOpt match {
          case None =>
            loop(
              restOfRows,
              rowIdentifiers,
              rowDescriptionToRowsHashMap
            )

          case Some(orderedRowDescription) =>
            loop(
              restOfRows,
              rowIdentifiers,
              rowDescriptionToRowsHashMap.addBinding(orderedRowDescription, row)
            )
        }
    }

    val rowDescriptionToRowsHashMap =
      new mutable.HashMap[OrderedRowDescription, mutable.Set[RawRow]]
      with mutable.MultiMap[OrderedRowDescription, RawRow]
    loop(rows, rowIdentifiers, rowDescriptionToRowsHashMap)
  }
}

object GroupRowsByRowDescription {

  private def apply(
      listOfRowIdentifier: List[RowToRowDescriptionMatcher],
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      quickState: QuickState
  ): Either[UnrecoverableError, GroupRowsByRowDescription] =
    listOfRowIdentifier match {
      case Nil =>
        GroupRowsByRowDescriptionErrorMessages.noRowDescriptionIsProvided
      case _ =>
        val groupRowsByRowDescription = new GroupRowsByRowDescription(
          listOfRowIdentifier,
          leftFileLabel,
          rightFileLabel,
          quickState)
        Right(groupRowsByRowDescription)
    }

  private def apply(
      fileDescriptionElem: Elem,
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      quickState: QuickState
  ): Either[UnrecoverableError, GroupRowsByRowDescription] = {

    @tailrec def loop(
        rowDescriptionsElems: List[Elem],
        rowIdentifierAccumulator: List[RowToRowDescriptionMatcher]
    ): Either[UnrecoverableError, List[RowToRowDescriptionMatcher]] =
      rowDescriptionsElems match {
        case Nil =>
          Right(rowIdentifierAccumulator)

        case rowDesc :: restOfRowDescriptions =>
          val rowIdentifier = RowToRowDescriptionMatcher(rowDesc)
          rowIdentifier match {
            case Left(errorMessage) =>
              Left(errorMessage)
            case Right(validRowIdentifier) =>
              loop(restOfRowDescriptions,
                   validRowIdentifier :: rowIdentifierAccumulator)
          }
      }

    val listOfRowIdentifier =
      loop(XMLHelperFunctions.collectElemChildren(fileDescriptionElem), Nil)
    listOfRowIdentifier match {
      case Right(list) =>
        GroupRowsByRowDescription(list,
                                  leftFileLabel,
                                  rightFileLabel,
                                  quickState)
      case Left(errorMessage) =>
        GroupRowsByRowDescriptionErrorMessages.invalidFileDescriptionFile(
          errorMessage)
    }

  }

  def apply(
      fileDescriptionRootElem: Elem,
      parserId: Option[String],
      leftFileLabel: Option[String],
      RightFileLabel: Option[String],
      quickState: QuickState
  ): Either[UnrecoverableError, GroupRowsByRowDescription] = {
    val fileDescriptionElem =
      getFileDescriptionElem(fileDescriptionRootElem, parserId)
    fileDescriptionElem match {
      case Right(fileDescElem) =>
        GroupRowsByRowDescription(fileDescElem,
                                  leftFileLabel,
                                  RightFileLabel,
                                  quickState)
      case Left(errorMessage) =>
        GroupRowsByRowDescriptionErrorMessages.invalidFileDescriptionFile(
          errorMessage)
    }
  }

  private def getFileDescriptionElem(
      fileDescriptionRootElem: Elem,
      parserIdOption: Option[String]
  ): Either[UnrecoverableError, Elem] = {
    val fileDescriptionsList = getFileDescriptionsList(fileDescriptionRootElem)
    fileDescriptionsList match {
      case Left(errorMessage) =>
        Left(errorMessage)
      case Right(listOfElems) =>
        getFileDescriptionWithId(listOfElems, parserIdOption)
    }
  }

  private def getFileDescriptionWithId(
      listOfFileDescriptions: List[Elem],
      providedIdOption: Option[String]
  ): Either[UnrecoverableError, Elem] = {
    @tailrec def loop(
        listOfFileDescriptions: List[Elem],
        providedId: String
    ): Either[UnrecoverableError, Elem] = listOfFileDescriptions match {
      case Nil =>
        GroupRowsByRowDescriptionErrorMessages.noFileDescriptionElemsForId(
          providedId)
      case fileDescElem :: restOfElems
          if XMLHelperFunctions.haveLabel(fileDescElem,
                                          fileDescriptionElemLabel) =>
        val fileDescIdOpt = getId(fileDescElem.attributes)
        fileDescIdOpt match {
          case Right(fileDescId) if fileDescId == providedId =>
            Right(fileDescElem)
          case Left(error: BunchOfErrors) =>
            GroupRowsByRowDescriptionErrorMessages
              .invalidUnorderedFileDescriptionAttributes(error)
          case _ =>
            loop(restOfElems, providedId)
        }
      case elem :: _ =>
        GroupRowsByRowDescriptionErrorMessages
          .unknownFileDescriptionsListChildElem(elem) // dead code
    }

    (listOfFileDescriptions, providedIdOption) match {
      case (Nil, _) =>
        GroupRowsByRowDescriptionErrorMessages.noFileDescriptionElemIsFound
      case (fileDescElem :: Nil, None) =>
        Right(fileDescElem)
      case (_, None) =>
        GroupRowsByRowDescriptionErrorMessages.noIdProvided
      case (_, Some(providedId)) =>
        loop(listOfFileDescriptions, providedId)
    }
  }

  private def getId(
      fileDescMetaData: MetaData): Either[UnrecoverableError, String] = {
    val unknownAttributes = XMLHelperFunctions.collectUnknownAttributes(
      fileDescriptionAttributeKeysList,
      fileDescMetaData)
    unknownAttributes match {
      case Nil =>
        val classParameters = AttributesValuesExtractor(
          fileDescMetaData,
          fileDescriptionAttributeKeysList)
        classParameters.get(fileDescriptionIdAttributeKey)
      case _ =>
        val bunchOfErrors = BunchOfErrors(unknownAttributes)
        Left(bunchOfErrors)
    }
  }

  private def getFileDescriptionsList(
      fileDescRootElem: Elem
  ): Either[UnrecoverableError, List[Elem]] =
    if (XMLHelperFunctions.haveLabel(fileDescRootElem,
                                     fileDescriptionElemLabel)) {
      Right(List(fileDescRootElem))
    } else if (XMLHelperFunctions.haveLabel(fileDescRootElem,
                                            fileDescriptionsListElemLabel)) {
      validateFileDescriptionsListAndGetChildElems(fileDescRootElem)
    } else {
      GroupRowsByRowDescriptionErrorMessages.unknownFileDescriptionRootElem(
        fileDescRootElem)
    }

  private def validateFileDescriptionsListAndGetChildElems(
      fileDescriptionsListElem: Elem
  ): Either[UnrecoverableError, List[Elem]] = {
    val unknownAttributes = XMLHelperFunctions.collectUnknownAttributes(
      fileDescriptionsListAttributeKeysList,
      fileDescriptionsListElem.attributes)
    unknownAttributes match {
      case Nil =>
        val descriptionListChildElem =
          XMLHelperFunctions.collectElemChildren(fileDescriptionsListElem)
        validateFileDescriptionChildElems(descriptionListChildElem)

      case _ =>
        val bunchOfErrors = BunchOfErrors(unknownAttributes)
        GroupRowsByRowDescriptionErrorMessages
          .invalidUnorderedFilesDescriptionsListAttributes(bunchOfErrors)
    }
  }

  private def validateFileDescriptionChildElems(
      childElems: List[Elem]
  ): Either[UnrecoverableError, List[Elem]] = {
    def collectUnknownChildElems(childElems: List[Elem]): List[Elem] =
      childElems.filter {
        !XMLHelperFunctions.haveLabel(_, fileDescriptionElemLabel)
      }

    val unknownChildElems = collectUnknownChildElems(childElems)
    unknownChildElems match {
      case Nil =>
        Right(childElems)
      case _ =>
        val unknownChildElemsErrorMessages = unknownChildElems.map(
          GroupRowsByRowDescriptionErrorMessages
            .unknownFileDescriptionsListChildElemError(_)
        )
        Left(BunchOfErrors(unknownChildElemsErrorMessages))
    }

  }

  private val fileDescriptionsListElemLabel = "FileDescriptionsList"
  private val fileDescriptionElemLabel = "UnorderedFileDescription"
  private val fileDescriptionIdAttributeKey =
    AttributeValueExtractor("Id", AttributeValueConversion.extractValue)
  private val fileDescriptionAttributeKeysList = List(
    fileDescriptionIdAttributeKey)
  private val fileDescriptionsListAttributeKeysList = Nil
}
