package com.scalableQuality.quick.core.others.errorMessages

import com.scalableQuality.quick.mantle.error.{
  DependencyError,
  UnrecoverableError
}

object ValueMapperErrorMessages {
  val doing = "validating value mapping attributes"

  def invalidAttributes(encounteredErrors: List[UnrecoverableError]) = {
    val errorMessage = DependencyError(
      doing,
      encounteredErrors
    )
    Left(errorMessage)
  }

}
