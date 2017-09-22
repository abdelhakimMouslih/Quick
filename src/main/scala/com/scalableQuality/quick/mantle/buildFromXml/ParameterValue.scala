package com.scalableQuality.quick.mantle.buildFromXml

import com.scalableQuality.quick.mantle.log.ErrorMessage

//TODO add an identification message in case of error
sealed trait ParameterValue[ValueType]

sealed trait ParameterValueFound[ValueType] extends ParameterValue[ValueType]

sealed trait ParameterValueError[ValueType] extends ParameterValue[ValueType] {
  val errorMessage: ErrorMessage
}

object ParameterValueFound {
  def apply[ValueType](
            convertedValue: Either[ErrorMessage,ValueType]
           ): ParameterValueFound[ValueType] = convertedValue match {
    case Right(value) => ValidParameterValueFound(value)
    case Left(errorMessage) => InvalidParameterValueFound(errorMessage)
  }
}

case class ValidParameterValueFound[ValueType](
                                 value: ValueType
                                 ) extends ParameterValueFound[ValueType]


case class InvalidParameterValueFound[ValueType](
                                       error: ErrorMessage
                                     ) extends {
  override val errorMessage = error
} with ParameterValueFound[ValueType] with ParameterValueError[ValueType]


case class ParameterValueNotFound[ValueType](
                                   error: ErrorMessage
                                 ) extends {
  override val errorMessage = error
} with ParameterValueError[ValueType]
