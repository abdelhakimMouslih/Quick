package com.scalableQuality.quick.mantle.parsing

import java.util.regex.Pattern

import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

class LiteralDelimiter(
                        value: String
                      ) {
  // -1 is to not disregard trailing columns,
  // look here http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#split(java.lang.String,%20int)
  def splitRow(row: RawRow): Vector[String] = row.value.split(value, -1).toVector
}

object LiteralDelimiter {
  //Pattern.quote(value) to quote regex expressions
  def apply(value: String ): Either[ErrorMessage,LiteralDelimiter] = if(value.length > 0){
    val delimiter = new LiteralDelimiter(Pattern.quote(value))
    Right(delimiter)
  } else {
    val errorMessage = UnrecoverableError(
      "creating a delimiter",
      "delimiter is empty",
      "provide a non empty delimiter"
    )
    Left(errorMessage)
  }
}
