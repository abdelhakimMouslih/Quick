package com.scalableQuality.quick.core.fileComponentDescripts.errorMessages

import com.scalableQuality.quick.mantle.error.{DependencyError, UnrecoverableError}

object FixedColumnDescriptionErrorMessages {

  val invalidColumnDescriptionWithoutLabel = "Invalid ColumnDescription no label is provided"
  def invalidColumnDescriptionWithLabel(label: String) = s"""Invalid ColumnDescription with label "${label}" """

  def invalidAttributes(label: String, encounteredErrors: List[UnrecoverableError]) = {
    val errorMessages = DependencyError(
      invalidColumnDescriptionWithLabel(label),
      encounteredErrors
    )
    Left(errorMessages)
  }

  def invalidAttributes(encounteredErrors: List[UnrecoverableError]) = {
    val errorMessages = DependencyError(
      invalidColumnDescriptionWithoutLabel,
      encounteredErrors
    )
    Left(errorMessages)
  }
}
