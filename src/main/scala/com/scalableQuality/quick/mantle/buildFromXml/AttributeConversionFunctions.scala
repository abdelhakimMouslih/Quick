package com.scalableQuality.quick.mantle.buildFromXml

import java.util.regex.Pattern

import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.collection.mutable
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try, Either, Right, Left}
import scala.xml.{Attribute, MetaData, Null}

object AttributeConversionFunctions {

  def toPattern(attribute: Attribute): Either[ErrorMessage, Pattern] = {
    val attributeValue = valueOption(attribute).getOrElse("")
    safePatternCompilation(attributeValue)
  }

  def toInt(attribute: Attribute): Either[ErrorMessage, Int] = {
    val attributeValue = valueOption(attribute).getOrElse("")
    safeStringToInt(attributeValue)
  }

  def toBoolean(attribute: Attribute): Either[ErrorMessage, Boolean] = {
    val attributeValue = valueOption(attribute).getOrElse("")
    safeStringToBoolean(attributeValue)
  }

  def extractValue(attribute: Attribute): Either[ErrorMessage, String] = valueOption(attribute) match {
    case Some(value) => Right(value)
    case None =>
      val errMessage = UnrecoverableError(
        s"""extracting value of attribute ${attribute.key}""",
        s"""no value found""",
        s"""supply a value for attribute ${attribute.key}"""
    )
      Left(errMessage)
  }

  private def valueOption(attribute: Attribute): Option[String] =
    attribute.value.flatMap(_.headOption).map(_.text).headOption

  private def safePatternCompilation(pattern: String ) : Either[ErrorMessage, Pattern] = Try(Pattern.compile(pattern)) match {
    case Success(pat) => Right(pat)
    case Failure(throwable) =>
      val errMessage = UnrecoverableError(
        s"""converting $pattern to a regex""",
        throwable.getMessage,
        s"""replace "$pattern" with a valid regex"""
      )
      Left(errMessage)
  }


  private def safeStringToInt(number: String): Either[ErrorMessage, Int] = Try(number.trim.toInt) match {
    case Success(num) => Right(num)
    case Failure(throwable) =>
      val errMessage = UnrecoverableError(
        s"""converting $number to Int""",
        throwable.getMessage,
        s"""replace "$number" with a valid integer """
      )
      Left(errMessage)
  }


  private def safeStringToBoolean(boolean: String): Either[ErrorMessage, Boolean] = Try(boolean.trim.toLowerCase.toBoolean) match {
    case Success(bool) => Right(bool)
    case Failure(thorwable) =>
      val errMessage = UnrecoverableError(
      s"""converting "$boolean" to boolean""",
      thorwable.getMessage,
      s"""replace "$boolean" with either "true" or "false" """
    )
      Left(errMessage)
  }

}




