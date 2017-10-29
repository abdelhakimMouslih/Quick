package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error.{BunchOfErrors, DependencyError, UnrecoverableError}

object FixedColumnIdentifierErrorMessages {
  val actionDescriptionWithoutLabel = "validating attributes of column identifier"

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
