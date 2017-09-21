package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.mantle.buildFromXml.{ParameterValueError, _}
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.xml.MetaData

class FixedLengthPosition(
                            startsAt: Int,
                            endsAt: Int,
                            override val toString: String
                          ) extends  ColumnPosition {

  override def extractColumnValue(row: RawRow): Option[String] = try {
    Some(row.value.substring(startsAt, endsAt))
  } catch {
    case e:Exception => None
  }

}

object FixedLengthPosition {

  def apply(startsAt: Int, endsAt: Int, length: Int): FixedLengthPosition = {
    val toString = "%3d - %3d, len:%3d".format(startsAt, endsAt, length)
    new FixedLengthPosition(startsAt, endsAt, toString)
  }

  def apply(elemAttributes:MetaData): Either[ErrorMessage, FixedLengthPosition] = {

    val classParameters:XMLAttributesToClassParameters =
      XMLAttributesToClassParameters(elemAttributes, parametersAttributeKeys)

    val startsAtParameterValue = classParameters.get(startsAtKey)
    val endsAtParameterValue = classParameters.get(endsAtKey)
    val lengthParameterValue = classParameters.get(lengthKey)

    FixedLengthPosition(startsAtParameterValue, endsAtParameterValue, lengthParameterValue)
  }

  def apply(
           startsAtParameterValue: ParameterValue[Int],
           endsAtParameterValue: ParameterValue[Int],
           lengthParameterValue: ParameterValue[Int]
           ): Either[ErrorMessage, FixedLengthPosition] = {
    val startsAtValidated = validateStartsAt(startsAtParameterValue)
    val lengthValidated = validateLength(endsAtParameterValue)
    (startsAtValidated, lengthValidated) match {
      case (Right(startsAt), Right(length)) =>
        val endsAt = calculateEndsAs(startsAt, length)
        Right(FixedLengthPosition(startsAt, endsAt, length))
      case (Right(startsAt), Left(lengthErrorMessage)) =>
        val endsAtValidated = validateEndsAt(startsAt, endsAtParameterValue)
        endsAtValidated match {
          case Right(endsAt) =>
            val length = calculateLength(startsAt, endsAt)
            Right(FixedLengthPosition(startsAt, endsAt, length))
          case Left(endsAtErrorMessage) =>
            connotMakeFixedLengthPosition(endsAtErrorMessage)
        }
      case (Left(errorMessage),_) =>
        connotMakeFixedLengthPosition(errorMessage)
    }
  }

  private def validateStartsAt(startsAtParameter : ParameterValue[Int]): Either[ErrorMessage, Int] =
    startsAtParameter match {
      case parameterValueError: ParameterValueError[_] =>
        Left(parameterValueError.errorMessage)
      case ValidParameterValueFound(startsAt: Int) if startsAt < 1 =>
        val errMessage = UnrecoverableError(
          "verifying startsAt Value",
          "value of attribute startsAt cannot be > 1",
          "supply a startsAt value > 1"
        )
        Left(errMessage)
      case ValidParameterValueFound(startsAt: Int) =>
        Right(startsAt)
    }


  private def validateEndsAt(startsAt: Int, endsAtParameterValue: ParameterValue[Int]): Either[ErrorMessage, Int] =
    endsAtParameterValue match {
      case parameterValueError: ParameterValueError[_] =>
        Left(parameterValueError.errorMessage)
      case ValidParameterValueFound(endsAt) if endsAt < startsAt =>
        val errMessage = UnrecoverableError(
          "verifying endsAt value",
          "value of attribute endsAt cannot be < startsAt",
          "supply an endsAt value > startsAt"
        )
        Left(errMessage)
      case ValidParameterValueFound(endsAt) =>
        Right(endsAt)
    }

  private def validateLength(lengthParameterValue: ParameterValue[Int]): Either[ErrorMessage, Int] =
    lengthParameterValue match {
      case parameterValueError: ParameterValueError[_] =>
        Left(parameterValueError.errorMessage)
      case ValidParameterValueFound(length) if length < 1 =>
        val errMessage = UnrecoverableError(
          "verifying length value",
          "value of attribute length cannot be < 1",
          "supply an length value > 1"
        )
        Left(errMessage)
      case ValidParameterValueFound(length) =>
        Right(length)
    }



  private def connotMakeFixedLengthPosition(errorMessage: ErrorMessage): Either[ErrorMessage, FixedLengthPosition] = Left(
    UnrecoverableError(
      "creating a fixed Length position",
      "startsAt, endsAt and length should contain valid values",
      "correct the problems mentioned below",
      List(errorMessage)
    )
  )


  private def calculateLength(startsAt: Int, endsAt: Int) = endsAt - startsAt + 1
  private def calculateEndsAs(startsAt: Int, length: Int) = startsAt + length - 1

  private val startsAtKey = ParameterAttribute("startsAt", AttributeConversionFunctions.toInt)
  private val endsAtKey = ParameterAttribute("endsAt", AttributeConversionFunctions.toInt)
  private val lengthKey = ParameterAttribute("length", AttributeConversionFunctions.toInt)
  private val parametersAttributeKeys = List(startsAtKey, endsAtKey, lengthKey)
}
