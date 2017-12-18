package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  DependencyError,
  EncounteredError,
  UnrecoverableError
}

import scala.xml.Elem

object GroupRowsByRowDescriptionErrorMessages {

  def unknownRow(filePath: String,
                 xmlFilePath: String,
                 fileDescriptionIdOpt: Option[String],
                 rowLine: Int) = {
    val fileDescriptionId = fileDescriptionIdOpt.getOrElse("N/A")
    val errorMessages = EncounteredError(
      s"parsing file $filePath",
      s"no row description in file description (file: $xmlFilePath, id : $fileDescriptionId) can be applied to the row at line $rowLine",
      s"use the -u flag to ignore undescribed rows, or create a row description."
    )
    Left(errorMessages)
  }

  def noRowDescriptionIsProvided = {
    val errorMessage = EncounteredError(
      "validating the xml file description",
      "no *OrderedRowDescription elem is provided in the targeted file description",
      "please provide either a FixedOrderedRowDescription or DelimitedOrderedRowDescription"
    )
    Left(errorMessage)
  }

}
