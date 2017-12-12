package com.scalableQuality.quick.core.others

import java.util.regex.Pattern

import com.scalableQuality.quick.core.others.errorMessages.MatchAgainstErrorMessages
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.xml.MetaData

class MatchAgainst(
    pattern: Pattern
) {
  def apply(value: String): Boolean = pattern.matcher(value).matches()
}

object MatchAgainst {
  def apply(
      pattern: Pattern
  ): MatchAgainst = new MatchAgainst(pattern)

  def apply(
      elemMetaData: MetaData): Either[UnrecoverableError, MatchAgainst] = {
    val classParameterFromAttributes =
      AttributesValuesExtractor(elemMetaData, matchAgainstAttributeKey)
    val patternValue =
      classParameterFromAttributes.get(matchAgainstAttributeKey)
    patternValue match {
      case Right(pattern) =>
        val matchAgainst = MatchAgainst(pattern)
        Right(matchAgainst)
      case Left(errorMessage) =>
        MatchAgainstErrorMessages.invalidAttributes(errorMessage)
    }
  }
  private val matchAgainstAttributeKey =
    AttributeValueExtractor("matchAgainst", AttributeValueConversion.toPattern)
  val listOfAttributesKeys = List(matchAgainstAttributeKey)
}
