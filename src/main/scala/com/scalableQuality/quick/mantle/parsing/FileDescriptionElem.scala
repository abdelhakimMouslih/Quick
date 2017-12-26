package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.mantle.constructFromXml.{
  AttributeValueConversion,
  AttributeValueExtractor,
  AttributesValuesExtractor,
  XMLHelperFunctions
}
import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  UnrecoverableError
}
import com.scalableQuality.quick.mantle.parsing.errorMessages.FileDescriptionElemErrorMessages

import scala.annotation.tailrec
import scala.xml.{Elem, MetaData}

class FileDescriptionElem(
    val elem: Either[UnrecoverableError, Elem],
    val xmlPath: String,
    val descriptionId: Option[String]
)

object FileDescriptionElem {

  def apply(fileDescriptionRootElem: Elem,
            filePath: String,
            parserIdOption: Option[String]): FileDescriptionElem = {
    val element: Either[UnrecoverableError, Elem] =
      getFileDescriptionElem(fileDescriptionRootElem, parserIdOption).fold(
        FileDescriptionElemErrorMessages
          .ErrorValidatingFileDescription(filePath, parserIdOption, _),
        Right(_)
      )
    new FileDescriptionElem(element, filePath, parserIdOption)
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
        FileDescriptionElemErrorMessages.noFileDescriptionElemsForId(providedId)
      case fileDescElem :: restOfElems
          if XMLHelperFunctions.haveLabel(fileDescElem,
                                          fileDescriptionElemLabel) =>
        val fileDescIdOpt = getId(fileDescElem.attributes)
        fileDescIdOpt match {
          case Right(fileDescId) if fileDescId == providedId =>
            Right(fileDescElem)
          case Left(error: BunchOfErrors) =>
            FileDescriptionElemErrorMessages
              .invalidUnorderedFileDescriptionAttributes(error)
          case _ =>
            loop(restOfElems, providedId)
        }
      case elem :: _ =>
        FileDescriptionElemErrorMessages
          .unknownFileDescriptionsListChildElem(elem) // dead code
    }

    (listOfFileDescriptions, providedIdOption) match {
      case (Nil, _) =>
        FileDescriptionElemErrorMessages.noFileDescriptionElemIsFound
      case (fileDescElem :: Nil, None) =>
        Right(fileDescElem)
      case (_, None) =>
        FileDescriptionElemErrorMessages.noIdProvided
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
      FileDescriptionElemErrorMessages.unknownFileDescriptionRootElem(
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
        FileDescriptionElemErrorMessages
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
          FileDescriptionElemErrorMessages
            .unknownFileDescriptionsListChildElemError(_)
        )
        Left(BunchOfErrors(unknownChildElemsErrorMessages))
    }

  }

  private val fileDescriptionsListElemLabel = "FilesDescriptionsList"
  private val fileDescriptionElemLabel = "UnorderedFileDescription"
  private val fileDescriptionIdAttributeKey =
    AttributeValueExtractor("Id", AttributeValueConversion.extractValue)
  private val fileDescriptionAttributeKeysList = List(
    fileDescriptionIdAttributeKey)
  private val fileDescriptionsListAttributeKeysList = Nil
}
