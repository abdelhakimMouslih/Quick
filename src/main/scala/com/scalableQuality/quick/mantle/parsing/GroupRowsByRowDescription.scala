package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.Reporting.ValidationAndMatchingReport
import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.core.fileProcessing.{ValidateAndMatchRows, ValidateAndMatchTwoFiles}
import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.annotation.tailrec
import scala.xml.{Elem, MetaData, Node}
import scala.collection.mutable

class GroupRowsByRowDescription(
                                val listOfRowIdentifier : List[RowToRowDescriptionMatcher],
                                leftFileLabel: Option[String],
                                rightFileLabel: Option[String]
                                ) {
  def validateAndMatchTheseTwoFiles(
                                     leftFileRows : => List[RawRow],
                                     rightFileRows: => List[RawRow]
                                   ): ValidateAndMatchTwoFiles = {
    val groupLeftFileRowsByRowDescription = groupRowsByRowDescription(leftFileRows, listOfRowIdentifier)
    val groupRightFileRowsByRowDescription = groupRowsByRowDescription(rightFileRows, listOfRowIdentifier)
    val matchedGroups = matchGroupsRowsByRowDescription(groupLeftFileRowsByRowDescription, groupRightFileRowsByRowDescription)
    val validationAndMatchingProcesses: List[() => ValidationAndMatchingReport] = matchedGroups.map {
      validationAndMatchingParameters =>
        lazyValidationAndMatchingProcesses(
          validationAndMatchingParameters._1,
          validationAndMatchingParameters._2,
          validationAndMatchingParameters._3
        )
    }
    ValidateAndMatchTwoFiles(validationAndMatchingProcesses)
  }

  private def lazyValidationAndMatchingProcesses(
                                                  orderedRowDescription: OrderedRowDescription,
                                                  leftFileRows: List[RawRow],
                                                  rightFileRows: List[RawRow],
                                                  leftFileLabel: Option[String] = this.leftFileLabel,
                                                  rightFileLabel: Option[String] = this.rightFileLabel
                                                ): () => ValidationAndMatchingReport =
    () => ValidateAndMatchRows(
      orderedRowDescription,
      leftFileRows,
      rightFileRows,
      leftFileLabel,
      rightFileLabel
    )

  private def matchGroupsRowsByRowDescription(
                                       leftFileRowGroups : List[(OrderedRowDescription, List[RawRow])],
                                       rightFileRowGroups: List[(OrderedRowDescription, List[RawRow])]
                                     ): List[(OrderedRowDescription, List[RawRow], List[RawRow])] = {
    @tailrec def loop(
                       leftFileRowGroups : List[(OrderedRowDescription, List[RawRow])],
                       rightFileRowGroups: List[(OrderedRowDescription, List[RawRow])],
                       accumulator : List[(OrderedRowDescription, List[RawRow], List[RawRow])]
                     ): List[(OrderedRowDescription, List[RawRow], List[RawRow])] = leftFileRowGroups match {
      case Nil =>
        val remainingRightFileRowGroups = rightFileRowGroups.map(rowGroup => (rowGroup._1, Nil, rowGroup._2))
        remainingRightFileRowGroups ::: accumulator

      case (orderedRowDescription, leftFileRows)::restOfLeftFileRowGroups =>
        val (matchingRightGroups, restOfRightFileRowGroups) = rightFileRowGroups.partition(_._1 == orderedRowDescription)
        val groupedByRowDescription = matchingRightGroups match {
          case Nil =>
            (orderedRowDescription,leftFileRows, Nil)

          case (_, rightFileRows)::_ =>
            (orderedRowDescription,leftFileRows, rightFileRows)
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

    type RowDescriptionToRowsHashMap = mutable.HashMap[OrderedRowDescription, mutable.Set[RawRow]] with mutable.MultiMap[OrderedRowDescription, RawRow]

    @tailrec def loop(
                       rows: List[RawRow],
                       rowIdentifiers: List[RowToRowDescriptionMatcher],
                       rowDescriptionToRowsHashMap: RowDescriptionToRowsHashMap
                     ) : List[(OrderedRowDescription, List[RawRow])] = rows match {
      case Nil =>
        val rowDescriptionAndRows = for {
          orderedRowAndItsRows <- rowDescriptionToRowsHashMap
        } yield (orderedRowAndItsRows._1, orderedRowAndItsRows._2.toList)
        rowDescriptionAndRows.toList

      case row::restOfRows =>
        val orderedRowDescriptionOpt = rowIdentifiers.collectFirst{
          case rowIdentifier if rowIdentifier.canIdentify(row) => rowIdentifier.orderedRowDescription
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
      new mutable.HashMap[OrderedRowDescription, mutable.Set[RawRow]] with mutable.MultiMap[OrderedRowDescription, RawRow]
    loop(rows, rowIdentifiers, rowDescriptionToRowsHashMap)
  }
}














object GroupRowsByRowDescription {

  private def apply(
                     listOfRowIdentifier: List[RowToRowDescriptionMatcher],
                     leftFileLabel: Option[String],
                     rightFileLabel: Option[String]
           ): GroupRowsByRowDescription = new GroupRowsByRowDescription(listOfRowIdentifier, leftFileLabel, rightFileLabel)

  private def apply(
             fileDescriptionElem: Elem,
             leftFileLabel: Option[String],
             rightFileLabel: Option[String]
           ): Either[ErrorMessage, GroupRowsByRowDescription] = {

    @tailrec def loop(
                       rowDescriptionsElems: List[Elem],
                       rowIdentifierAccumulator: List[RowToRowDescriptionMatcher]
                     ): Either[ErrorMessage, List[RowToRowDescriptionMatcher]] = rowDescriptionsElems match {
      case Nil =>
        Right(rowIdentifierAccumulator)

      case rowDesc::restOfRowDescriptions =>
        val rowIdentifier = RowToRowDescriptionMatcher(rowDesc)
        rowIdentifier match {
          case Left(errorMessage) =>
            Left(errorMessage)
          case Right(validRowIdentifier) =>
            loop(restOfRowDescriptions, validRowIdentifier::rowIdentifierAccumulator)
        }
    }


    val listOfRowIdentifier = loop(collectAllChildElems(fileDescriptionElem), Nil)
    listOfRowIdentifier match {
      case Right(list) =>
        val rowToRowDescriptionMatcher = GroupRowsByRowDescription(list, leftFileLabel, rightFileLabel)
        Right(rowToRowDescriptionMatcher)
      case Left(errorMessage) =>
        cannotMake(errorMessage)
    }

  }

  def apply(
             fileDescriptionRootElem: Elem,
             parserId: Option[String],
             leftFileLabel : Option[String],
             RightFileLabel : Option[String]
           ): Either[ErrorMessage, GroupRowsByRowDescription] = {
    val fileDescriptionElem = getFileDescriptionElem(fileDescriptionRootElem, parserId)
    fileDescriptionElem match {
      case Right(fileDescElem) =>
        GroupRowsByRowDescription(fileDescElem,leftFileLabel,RightFileLabel)
      case Left(errorMessage) =>
        cannotMake(errorMessage)
    }
  }

  private def getFileDescriptionElem(
                                      fileDescriptionRootElem: Elem,
                                      parserIdOption: Option[String]
                                    ): Either[ErrorMessage, Elem] = {
    val fileDescriptionsList = getFileDescriptionsList(fileDescriptionRootElem)
    fileDescriptionsList match {
      case Left(errorMessage) =>
        Left(errorMessage)
      case Right(listOfElems) =>
        val fileDescriptionElem = getFileDescriptionWithId(listOfElems, parserIdOption)
        fileDescriptionElem
    }
  }

  private def getFileDescriptionWithId(
                                        listOfFileDescriptions: List[Elem],
                                        providedIdOption: Option[String]
                                      ): Either[ErrorMessage, Elem] = {
    @tailrec def loop(
                       listOfFileDescriptions: List[Elem],
                       providedId: String
                     ): Either[ErrorMessage, Elem] = listOfFileDescriptions match {
      case Nil =>
        val errorMessage = UnrecoverableError(
          "looking for a file description xml element",
          "no file description found",
          "provide at least one file description to be used"
        )
        Left(errorMessage)
      case fileDescElem::restOfElems if hasLabel(fileDescElem, fileDescriptionElemLabel) =>
        val fileDescIdOpt = getId(fileDescElem.attributes)
        fileDescIdOpt match {
          case Some(fileDescId) if fileDescId == providedId =>
            Right(fileDescElem)
          case _ =>
            loop(restOfElems, providedId)
        }
      case elem::_ =>
        val errorMessage = UnrecoverableError(
          "creating file description",
          s"did not recognize ${elem.label}",
          "put in a valid elem"
        )
        Left(errorMessage)
    }


    (listOfFileDescriptions, providedIdOption)  match {
      case (Nil, _) =>
        val errorMessage = UnrecoverableError(
          "looking for a file description xml element",
          "no file description found",
          "provide at least one file description to be used"
        )
        Left(errorMessage)
      case ( (fileDescElem:Elem)::Nil, None) =>
        Right(fileDescElem)
      case (_, None) =>
        val errorMessage = UnrecoverableError(
          "looking for a file description xml element",
          "xml file contains multiple file description, and no id was specified",
          "specify the id of the file description you wish to use"
        )
        Left(errorMessage)
      case (_, Some(providedId)) =>
        loop(listOfFileDescriptions, providedId)
    }
  }



  private def getId(fileDescMetaData: MetaData): Option[String] = {
    val classParameters = XMLAttributesToClassParameters(fileDescMetaData, fileDescriptionAttributeKeysList)
    val idParameter = classParameters.get(fileDescriptionIdAttributeKey)
    idParameter match {
      case parameterValueError : ParameterValueError[_] =>
        None
      case ValidParameterValueFound(id) =>
        Some(id)
    }
  }


  private def collectAllChildElems(node: Node): List[Elem] = node.child.toList.collect{
    case elem: Elem => elem
  }

  private def getFileDescriptionsList(
                                       fileDescRootElem: Elem
                                     ): Either[ErrorMessage, List[Elem]] =
    if(hasLabel(fileDescRootElem, fileDescriptionElemLabel)) {
      Right(List(fileDescRootElem))
    } else if(hasLabel(fileDescRootElem, fileDescriptionsListElemLabel)) {
      Right(collectAllChildElems(fileDescRootElem))
    } else {
      val errorMessage = UnrecoverableError(
        "looking for a file description xml element",
        "connot find a file description elem nor a files descriptions list elem",
        "provide a fileDescription elem"
      )
      Left(errorMessage)
    }


  private def hasLabel(elem: Elem, label: String) : Boolean = elem.label.toLowerCase == label.toLowerCase()
  private def cannotMake(errorMessage: ErrorMessage): Either[ErrorMessage, GroupRowsByRowDescription] = {
    val cannotMakeErrorMessage = UnrecoverableError(
      "creating a file desceription",
      "encountered a problem",
      "please resolve the problems mentioned below",
      List(errorMessage)
    )
    Left(cannotMakeErrorMessage)
  }
  private val fileDescriptionsListElemLabel = "FileDescriptionsList"
  private val fileDescriptionElemLabel = "UnorderedFileDescription"
  private val fileDescriptionIdAttributeKey = ParameterAttribute("Id", AttributeConversionFunctions.extractValue)
  private val fileDescriptionAttributeKeysList = List(fileDescriptionIdAttributeKey)
}