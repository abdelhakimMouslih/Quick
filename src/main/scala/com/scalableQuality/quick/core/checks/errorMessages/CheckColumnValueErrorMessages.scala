package com.scalableQuality.quick.core.checks.errorMessages

import com.scalableQuality.quick.mantle.error.{DependencyError, UnrecoverableError}

object CheckColumnValueErrorMessages {
  def invalidChecksAttributesValues(errors: List[UnrecoverableError]) = {
    val errorMessage = DependencyError(
      "validating checks attributes",
      errors
    )
    Left(errorMessage)
  }
}
