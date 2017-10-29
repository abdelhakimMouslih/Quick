package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error.EncounteredError

object FixedRowIdentifierErrorMessages {
  val noColumnIdentifierIsProvided = {
    val errorMessage = EncounteredError(
      "validating columnIdentifier elements",
      "no columnIdentifier element was provided",
      "provide at least one columnIdentifier element"
    )
    Left(errorMessage)
  }
}
