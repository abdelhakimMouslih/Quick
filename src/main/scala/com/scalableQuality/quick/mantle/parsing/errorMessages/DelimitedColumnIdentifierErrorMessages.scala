package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error._

object DelimitedColumnIdentifierErrorMessages {
  val actionDescriptionWithoutLabel = "validating attributes of column identifier"

  def invalidAttributes(encounteredErrors: List[UnrecoverableError]) = {
    val errorMessage = DependencyError(
      actionDescriptionWithoutLabel,
      encounteredErrors
    )
    Left(errorMessage)
  }
}
