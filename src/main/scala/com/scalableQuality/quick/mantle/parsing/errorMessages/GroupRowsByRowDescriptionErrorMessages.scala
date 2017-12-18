package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  DependencyError,
  EncounteredError,
  UnrecoverableError
}

import scala.xml.Elem

object GroupRowsByRowDescriptionErrorMessages {

  def unknownRow(filePath: String, xmlFilePath: String, fileDescriptionIdOpt: Option[String], rowLine: Int) = {
    val fileDescriptionId = fileDescriptionIdOpt.getOrElse("N/A")
    val errorMessages = EncounteredError(
      s"parsing file $filePath",
      s"no row description in file description (file: $xmlFilePath, id : $fileDescriptionId) can be applied to the row at line $rowLine",
      s"use the -u flag to ignore undescribed rows, or create a row description."
    )
    Left(errorMessages)
  }

  private val validatingXmlActionDescription = "validating xml file description"
  private val validatingXmlHeadline = "invalid xml file description"

  def unknownFileDescriptionRootElem(rootElem: Elem) = {
    val errorMessage = EncounteredError(
      validatingXmlActionDescription,
      s"""${rootElem.label} is an unknown element""",
      "the root elem in the file description should be either FileDescriptionsList or UnorderedFileDescription"
    )
    Left(errorMessage)
  }

  def noFileDescriptionElemsForId(id: String) = {
    val errorMessage = EncounteredError(
      validatingXmlActionDescription,
      s"""no UnorderedFileDescription elem is found with id ${id}""",
      "please check that you provided an UnorderedFileDescription elem and the correct id"
    )
    Left(errorMessage)
  }

  val noFileDescriptionElemIsFound = {
    val errorMessage = EncounteredError(
      validatingXmlActionDescription,
      "no UnorderedFileDescription elem is found",
      "please check that you provided an UnorderedFileDescription"
    )
    Left(errorMessage)
  }

  val noIdProvided = {
    val errorMessage = EncounteredError(
      """validating FileDescriptionsList children elem""",
      "no id is provided and multiple UnorderedFileDescription are present in the file",
      "please provide the UnorderedFileDescription id using -i option from CLI"
    )
    Left(errorMessage)
  }

  def unknownFileDescriptionsListChildElemError(elem: Elem) = EncounteredError(
    validatingXmlActionDescription,
    s""""${elem.label}" is an unknown elem""",
    "please provide only UnorderedFileDescription inside FileDescriptionsList"
  )

  def unknownFileDescriptionsListChildElem(elem: Elem) = {
    val errorMessage = unknownFileDescriptionsListChildElemError(elem)
    Left(errorMessage)
  }

  def invalidFileDescriptionFile(errorMessages: UnrecoverableError*) = {
    val errorMessage = DependencyError(
      validatingXmlHeadline,
      errorMessages.toList
    )
    Left(errorMessage)
  }

  def noRowDescriptionIsProvided = {
    val errorMessage = EncounteredError(
      "validating the xml file description",
      "no *OrderedRowDescription elem is provided in the targeted file description",
      "please provide either a FixedOrderedRowDescription or DelimitedOrderedRowDescription"
    )
    invalidFileDescriptionFile(errorMessage)
  }

  def invalidUnorderedFileDescriptionAttributes(
      bunchOfErrors: BunchOfErrors) = {
    val errorMessage = DependencyError(
      "validating UnorderedFileDescription attributes",
      bunchOfErrors
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
