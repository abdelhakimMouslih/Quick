package com.scalableQuality.quick.core.others.errorMessages

import com.scalableQuality.quick.mantle.error.{
  DependencyError,
  UnrecoverableError
}

object MatchAgainstErrorMessages {
  val doing = "validating MatchAgainst attribute"

  def invalidAttributes(encounteredErrors: UnrecoverableError*) = {
    val errorMessage = DependencyError(
      doing,
      encounteredErrors.toList
    )
    Left(errorMessage)
  }
}
