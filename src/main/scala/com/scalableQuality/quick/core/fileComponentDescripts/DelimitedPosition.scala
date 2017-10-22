package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.MetaData

class DelimitedPosition(
                         position: Int,
                         printingPosition: Int
                       ) {
  override val toString: String = "%4d".format(printingPosition)
  def extractColumnValue(row: Vector[String]): Option[String] = row.lift(this.position)
}

object DelimitedPosition {
  def apply(
             position: Int
           ): Either[ErrorMessage, DelimitedPosition] =
    if (position > 0 ) {
      val delimitedPosition = new DelimitedPosition(calculateRelativePosition(position), position)
      Right(delimitedPosition)
    }
    else {
      val errorMessage = UnrecoverableError(
        "creating Delimited position",
        "position is <= 0",
        "position should be > 0"
      )
      Left(errorMessage)
    }


  def apply(
           metaData: MetaData
           ): Either[ErrorMessage, DelimitedPosition] = {
    val classParametersMap = XMLAttributesToClassParameters(metaData, positionAttribute)
    val positionParameter = classParametersMap.get(positionAttribute)
    positionParameter match {
      case error: ParameterValueError[_] =>
        makeError(error.errorMessage)

      case ValidParameterValueFound(position) =>
        DelimitedPosition(position)
    }
  }

  val positionAttribute = ParameterAttribute("position", AttributeConversionFunctions.toInt)

  def makeError(error: ErrorMessage): Either[ErrorMessage, DelimitedPosition] = {
    val errorMessage = UnrecoverableError(
      "creating Delimited position",
      "problem",
      "solve the problem",
      List(error)
    )
    Left(errorMessage)
  }

  private def calculateRelativePosition(positon: Int): Int = positon - 1
}