package com.scalableQuality.quick.core.fileComponentDescripts.errorMessages

import com.scalableQuality.quick.mantle.error.{DependencyError, UnrecoverableError}

object DelimitedColumnDescriptionErrorMessages {

  val actionDescriptionWithoutLabel = "validating attributes of column description"
  def actionDescriptionWithLabel(label: String) = s"""validating attributes of column description with label "${label}" """

  def invalidAttributes(label: String, encounteredErrors: List[UnrecoverableError]) = {
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
}
