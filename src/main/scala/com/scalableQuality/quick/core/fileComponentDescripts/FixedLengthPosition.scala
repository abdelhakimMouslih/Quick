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
    val mappedStartsAt = mapToClassParameter(startsAtParameterValue)
    val mappedEndsAt = mapToClassParameter(endsAtParameterValue)
    val mappedLength = mapToClassParameter(lengthParameterValue)
    (mappedStartsAt,mappedEndsAt,mappedLength) match {
      case (Left(errorMessage), _, _) =>
        Left(errorMessage)

      case (_,Left(errorMessage),_) =>
        Left(errorMessage)

      case (_,_,Left(errorMessage)) =>
        Left(errorMessage)

      case (Right(startsAt),Right(endsAt),Right(length)) =>
        FixedLengthPosition(startsAt, endsAt, length)
    }
  }




  private def apply(startsAt: Int, endsAt: Int, length: Int): FixedLengthPosition = {
    val actualStartsAt = startsAt - 1
    val toString = "%3d - %3d, len:%3d".format(startsAt, endsAt, length)
    new FixedLengthPosition(actualStartsAt, endsAt, toString)
  }

  private def apply(
           startsAt: Int,
           endsAtOpt: Option[Int],
           lengthOpt: Option[Int]
           ): Either[ErrorMessage, FixedLengthPosition] = if (startsAt < 1 ) {
    val errorMessage = UnrecoverableError(
      "verifying startsAt attribute value",
      "startsAt is less than one",
      "startsAt should be greater than one"
    )
    Left(errorMessage)
  } else {
    (endsAtOpt, lengthOpt) match {
      case (None, None) =>
        val errorMessage = UnrecoverableError(
          "verifying endsAt and length attributes",
          "endsAt and length attributes are missing",
          "specify either one"
        )
        Left(errorMessage)
      case (Some(endsAt), None) =>
        FixedLengthPositionWithEndsAt(startsAt, endsAt)

      case (None, Some(length)) =>
        FixedLengthPositionWithLength(startsAt, length)

      case (Some(endsAt), Some(length)) =>
        val calculatedLength = calculateLength(startsAt, endsAt)
        if (calculatedLength == length) {
          val fixedLengthPosition = FixedLengthPosition(startsAt, endsAt, length)
          Right(fixedLengthPosition)
        } else {
          val errorMessage = UnrecoverableError(
            "verifying endsAt and length attributes",
            "endsAt attribute value and length attribute value are not coherent",
            "correct that"
          )
          Left(errorMessage)
        }
    }
  }


  private def FixedLengthPositionWithEndsAt(
                                             startsAt: Int,
                                             endsAt: Int
                                           ): Either[ErrorMessage, FixedLengthPosition] = if (startsAt <= endsAt ){
    val length = calculateLength(startsAt, endsAt)
    val fixedLengthPosition = FixedLengthPosition(startsAt, endsAt, length)
    Right(fixedLengthPosition)
  } else {
    val errorMessage = UnrecoverableError(
      "verifying ends at value",
      "endsAt values is less than startsAt",
      "ends at value should be greater or equals to startsAt"
    )
    Left(errorMessage)
  }

  private def FixedLengthPositionWithLength(
                                           startsAt: Int,
                                           length: Int
                                           ): Either[ErrorMessage, FixedLengthPosition] = if (length  >= 1) {
    val endsAt = calculateEndsAs(startsAt, length)
    val fixedLengthPosition = FixedLengthPosition(startsAt, endsAt, length)
    Right(fixedLengthPosition)

  } else {
    val errorMessage = UnrecoverableError(
      "verifying length value",
      "length values is less than one",
      "ends at value should be greater or equals to one"
    )
    Left(errorMessage)
  }


  private def apply(
           startsAtOpt: Option[Int],
           endsAtOpt: Option[Int],
           lengthOpt: Option[Int]
           ): Either[ErrorMessage, FixedLengthPosition] = startsAtOpt match {
    case None =>
      val errorMessage = UnrecoverableError (
        "verifying startsAt attribute",
        "startsAt attribute is mandatory",
        "specify a startsAt attribute"
      )
      Left(errorMessage)


    case Some(startsAt) =>
      FixedLengthPosition(startsAt, endsAtOpt, lengthOpt)
  }


  private def mapToClassParameter(parameter: ParameterValue[Int]): Either[ErrorMessage, Option[Int]] = parameter match {
    case InvalidParameterValueFound(errorMessage) =>
      Left(errorMessage)

    case p :ParameterValueNotFound[Int] =>
      Right(None)

    case ValidParameterValueFound(param) =>
      val parameter = Some(param)
      Right(parameter)
  }

  private def calculateLength(startsAt: Int, endsAt: Int) = endsAt - startsAt + 1
  private def calculateEndsAs(startsAt: Int, length: Int) = startsAt + length - 2

  private val startsAtKey = ParameterAttribute("startsAt", AttributeConversionFunctions.toInt)
  private val endsAtKey = ParameterAttribute("endsAt", AttributeConversionFunctions.toInt)
  private val lengthKey = ParameterAttribute("length", AttributeConversionFunctions.toInt)
  private val parametersAttributeKeys = List(startsAtKey, endsAtKey, lengthKey)
}
