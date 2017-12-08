package com.scalableQuality.quick.mantle.constructFromXml

import java.util.regex.Pattern

import com.scalableQuality.quick.mantle.constructFromXml.errorMessages.AttributeValueConversionErrorMessages
import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.util.{Either, Failure, Right, Success, Try}
import scala.xml.Attribute

object AttributeValueConversion {


  def toPattern(attribute: Attribute): Either[UnrecoverableError, Pattern] =
    extractValueAndConvertTo(
      safePatternCompilation(_)
    )(
      attribute
    )

  def toInt(attribute: Attribute): Either[UnrecoverableError, Int] =
    extractValueAndConvertTo(
      safeStringToInt(_)
    )(
      attribute
    )

  def toBoolean(attribute: Attribute): Either[UnrecoverableError, Boolean] = extractValueAndConvertTo(
    safeStringToBoolean(_)
  )(
    attribute
  )

  def extractValue(attribute: Attribute): Either[UnrecoverableError, String] = extractValueAndConvertTo(
    Right(_)
  )(
    attribute
  )

  def extractValueAndConvertTo[ValueType](
                                           convert: String => Either[UnrecoverableError, ValueType]
                                         )
                                         (
                                           attribute: Attribute
                                         ): Either[UnrecoverableError, ValueType] = {
    extractAttributeValue(attribute) match {
      case Some(value) => convert(value)
      case None =>
        AttributeValueConversionErrorMessages.failedToExtractAttributeValue(attribute) // dead code
    }
  }


  private def extractAttributeValue(attribute: Attribute): Option[String] =
    attribute.value.flatMap(_.headOption).map(_.text).headOption

  private def safePatternCompilation(pattern: String ) : Either[UnrecoverableError, Pattern] = Try(Pattern.compile(pattern)) match {
    case Success(pat) => Right(pat)
    case Failure(throwable) =>
      AttributeValueConversionErrorMessages.failedToCompileRegex(pattern, throwable)
  }


  private def safeStringToInt(number: String): Either[UnrecoverableError, Int] = Try(number.trim.toInt) match {
    case Success(num) =>
      Right(num)

    case Failure(_) =>
      AttributeValueConversionErrorMessages.failedToConvertStringIntoAnInt(number)
  }


  private def safeStringToBoolean(boolean: String): Either[UnrecoverableError, Boolean] = Try(boolean.trim.toLowerCase.toBoolean) match {
    case Success(bool) =>
      Right(bool)

    case Failure(_) =>
      AttributeValueConversionErrorMessages.failedToConvertStringIntoABoolean(boolean)
  }

}
