package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  DependencyError,
  EncounteredError,
  UnrecoverableError
}
import com.scalableQuality.quick.mantle.parsing.FileDescriptionElem
import scala.xml.Elem

object FileDescriptionElemErrorMessages {

  def ErrorValidatingFileDescription[OutType](
      fileDescriptionElem: FileDescriptionElem,
      errors: UnrecoverableError): Either[UnrecoverableError, OutType] = {
    val fileDescId = fileDescriptionElem.descriptionId.getOrElse("N/A")
    val errorMessage = DependencyError(
      s"validating file description with id $fileDescId in file ${fileDescriptionElem.xmlPath}",
      errors
    )
    Left(errorMessage)
  }

  def ErrorValidatingFileDescription[OutType](
      path: String,
      id: Option[String],
      errors: UnrecoverableError): Either[UnrecoverableError, OutType] = {
    val fileDescId = id.getOrElse("N/A")
    val errorMessage = DependencyError(
      s"validating file description with id $fileDescId at $path",
      errors
    )
    Left(errorMessage)
  }

  def noFileDescriptionElemsForId(id: String) = {
    val errorMessage = EncounteredError(
      s"searching for an UnorderedFileDescription with id = $id",
      s"""no UnorderedFileDescription elem is found with id ${id}""",
      "please check that you provided an UnorderedFileDescription elem and the correct id"
    )
    Left(errorMessage)
  }

  def invalidUnorderedFileDescriptionAttributes(
      bunchOfErrors: BunchOfErrors) = {
    val errorMessage = DependencyError(
      "validating UnorderedFileDescription attributes",
      bunchOfErrors
    )
    Left(errorMessage)
  }

  def unknownFileDescriptionsListChildElemError(elem: Elem) = EncounteredError(
    "validating XML elements",
    s""""${elem.label}" is an unknown elem""",
    "please provide only UnorderedFileDescription inside FileDescriptionsList"
  )

  def unknownFileDescriptionsListChildElem(elem: Elem) = {
    val errorMessage = unknownFileDescriptionsListChildElemError(elem)
    Left(errorMessage)
  }

  val noFileDescriptionElemIsFound = {
    val errorMessage = EncounteredError(
      "validating XML elements",
      "no UnorderedFileDescription elem is found",
      "please check that you provided an UnorderedFileDescription"
    )
    Left(errorMessage)
  }

  val noIdProvided = {
    val errorMessage = EncounteredError(
      """searching for UnorderedFileDescription""",
      "no id is provided and multiple UnorderedFileDescription are present in the file",
      "please provide the UnorderedFileDescription id using -i option from CLI"
    )
    Left(errorMessage)
  }

  def unknownFileDescriptionRootElem(rootElem: Elem) = {
    val errorMessage = EncounteredError(
      """searching for UnorderedFileDescription""",
      s"""${rootElem.label} is an unknown element""",
      "the root elem in the file description should be either FileDescriptionsList or UnorderedFileDescription"
    )
    Left(errorMessage)
  }

  def invalidUnorderedFilesDescriptionsListAttributes(
      bunchOfErrors: BunchOfErrors) = {
    val errorMessage = DependencyError(
      "validating FileDescriptionsList attributes",
      bunchOfErrors
    )
    Left(errorMessage)
  }
}
