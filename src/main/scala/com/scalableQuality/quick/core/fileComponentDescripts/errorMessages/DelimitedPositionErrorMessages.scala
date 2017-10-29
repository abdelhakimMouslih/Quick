package com.scalableQuality.quick.core.fileComponentDescripts.errorMessages

import com.scalableQuality.quick.core.fileComponentDescripts.DelimitedPosition
import com.scalableQuality.quick.mantle.error.{DependencyError, EncounteredError, UnrecoverableError}


object DelimitedPositionErrorMessages {

  val actionDescription = "validating column position attribute"

  def invalidAttributes(encounteredError: UnrecoverableError): Either[UnrecoverableError, DelimitedPosition] = {
    val errorMessage = DependencyError(
      actionDescription,
      encounteredError
    )
    Left(errorMessage)
  }

  val positionIsLessThanOne : Either[UnrecoverableError, Int] = {
    val errorMessage = EncounteredError(
      actionDescription,
      "the provided position value is less than one",
      s"please provide a positon in the range of [1,${Int.MaxValue}]"
    )
    Left(errorMessage)
  }

}
