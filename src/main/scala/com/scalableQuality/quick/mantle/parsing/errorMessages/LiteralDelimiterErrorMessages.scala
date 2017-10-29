package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error.EncounteredError

object LiteralDelimiterErrorMessages {
  val invalidLiteralDelimiter = {
    val errorMessage = EncounteredError(
      "validating LiteralDelimiter attribute value",
      "the value of the LiteralDelimiter LiteralDelimiter is an empty string",
      "provide a delimiter string with length > 0"
    )
    Left(errorMessage)
  }
}
