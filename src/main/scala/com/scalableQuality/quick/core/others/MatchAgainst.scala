package com.scalableQuality.quick.core.others

import java.util.regex.Pattern

import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.MetaData

class MatchAgainst(
                    pattern: Pattern
                  ){
  def apply(value: String ): Boolean = pattern.matcher(value).matches()
}

object MatchAgainst {
  def apply(
             pattern: Pattern
           ): MatchAgainst = new MatchAgainst(pattern)

  def apply(elemMetaData: MetaData): Either[ErrorMessage, MatchAgainst] = {
    val classParameterFromAttributes = XMLAttributesToClassParameters(elemMetaData, matchAgainstAttributeKey)
    val matchAgainstClassParameter = classParameterFromAttributes.get(matchAgainstAttributeKey)
    matchAgainstClassParameter match {
      case parameterValueError : ParameterValueError[_] =>
        errorMessage(parameterValueError.errorMessage)
      case ValidParameterValueFound(pattern) =>
        Right(MatchAgainst(pattern))
    }
  }
  private val matchAgainstAttributeKey = ParameterAttribute("matchagainst", AttributeConversionFunctions.toPattern)
  def errorMessage(childErrorMessage: ErrorMessage): Left[ErrorMessage, MatchAgainst] = {
    val errorMessage = UnrecoverableError(
      "Creating matchAgainst object",
      "encountered a problem",
      "solve the problem described below",
      List(childErrorMessage)
    )
    Left(errorMessage)
  }
}