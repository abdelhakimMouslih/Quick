package com.scalableQuality.quick.core.fileComponentDescriptions.errorMessages

import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  DependencyError,
  UnrecoverableError
}

object DelimitedColumnDescriptionErrorMessages {

  val actionDescriptionWithoutLabel =
    "validating attributes of column description"
  def actionDescriptionWithLabel(label: String) =
    s"""validating attributes of column description with label "${label}" """

  def invalidAttributes(label: String,
                        encounteredErrors: List[UnrecoverableError]) = {
    val errorMessage = DependencyError(
      actionDescriptionWithLabel(label),
      encounteredErrors
    )
    Left(errorMessage)
  }

  def invalidAttributes(encounteredErrors: List[UnrecoverableError]) = {
    val errorMessage = DependencyError(
      actionDescriptionWithoutLabel,
      encounteredErrors
    )
    Left(errorMessage)
  }

  def invalidAttributes(encounteredErrors: BunchOfErrors) = {
    val errorMessage = DependencyError(
      actionDescriptionWithoutLabel,
      encounteredErrors
    )
    Left(errorMessage)
  }
}
