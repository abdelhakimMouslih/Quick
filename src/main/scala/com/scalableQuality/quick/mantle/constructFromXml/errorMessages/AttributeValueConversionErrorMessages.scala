package com.scalableQuality.quick.mantle.constructFromXml.errorMessages

import java.net.URL

import com.scalableQuality.quick.mantle.error._
import com.scalableQuality.quick.mantle.log.UnrecoverableError

import scala.util.Either
import scala.xml.Attribute

object AttributeValueConversionErrorMessages {

  def failedToConvertStringIntoAnInt(value: String) = {
      val errorMessage = InvalidAttributeValueError(
          s"""converting the following value "${value}" into an integer """,
          """the provided value is not an integer""",
          s"""provide an integer between ${Int.MinValue} and ${Int.MaxValue}"""
        )
    Left(errorMessage)
  }

  /*def failedToConvertAttributeValueToAnInt(attribute: Attribute, encounteredError: EncounteredError) = {
    val attributeKey = attribute.key
    val errorMessage = DependencyError(
      s"converting the value of ${attributeKey} attribute to an integer",
      encounteredError
    )
    Left(errorMessage)
  }*/

  def failedToConvertStringIntoABoolean(value: String) = {
    val errorMessage = InvalidAttributeValueError (
      s"""converting the following value "${value}" into a boolean """,
      """the provided value is not a boolean""",
      s"""provide one of these values ${true} or ${false}"""
    )
    Left(errorMessage)
  }

  /*def failedToConvertAttributeValueToABoolean(attribute: Attribute, encounteredError: EncounteredError) = {
    val attributeKey = attribute.key
    val errorMessage = DependencyError(
      s"converting the value of ${attributeKey} attribute to a boolean",
      encounteredError
    )
    Left(errorMessage)
  }*/

  def failedToCompileRegex(value: String, exception: Throwable) = {
    val javaRegexDocumentation = new URL("https://docs.oracle.com/javase/tutorial/essential/regex/")
    val errorMessage = InvalidAttributeValueError(
      s"""creating a regular expression from "${value}"""",
      s"""${exception.getMessage}""",
      s"""please visit ${javaRegexDocumentation} to learn more about regular expression"""
    )
    Left(errorMessage)
  }

  /*def failedToConvertAttributeValueToARegex(attribute: Attribute, encounteredError: EncounteredError) = {
    val attributeKey = attribute.key
    val errorMessage = DependencyError(
      s"creating a regex from the value of ${attributeKey} attribute",
      encounteredError
    )
    Left(errorMessage)
  }*/

  def failedToExtractAttributeValue(attribute: Attribute) = {
    val attributeKey = attribute.key
    val errorMessage = AttributeNotFoundError(
      s"""extracting the value of ${attributeKey} attribute """",
      """no value is found""",
      s"""provide an appropriate value for the ${attributeKey} attribute """
    )
    Left(errorMessage)
  }

}
