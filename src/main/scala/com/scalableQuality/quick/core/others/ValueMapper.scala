package com.scalableQuality.quick.core.others

import com.scalableQuality.quick.mantle.buildFromXml.{ParameterAttributeWithDefaultValue, _}
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.collection.mutable
import scala.xml.MetaData

class ValueMapper private[core] (
                   transformations: List[ValueMapperFunction]
                 ) {

  def apply(value: Option[String]): Option[String] = transformations.foldLeft(value) {
    (previousVal: Option[String], transformation: ValueMapperFunction) =>
      transformation(previousVal)
  }
  def contains(valueMapperFunction: ValueMapperFunction): Boolean = transformations.contains(valueMapperFunction)
}

object ValueMapper {

  def apply(listOfTransformation: List[ValueMapperFunction]): ValueMapper = {
    new ValueMapper(listOfTransformation)
  }

  // TODO : improve attribute extraction
  def comparisonValueMapper(elemMetaData: MetaData): Either[ErrorMessage, ValueMapper] = {

    val classParameters:XMLAttributesToClassParameters =
      XMLAttributesToClassParameters(elemMetaData, valueMapperAttributeKeys)
    val trimParameter = classParameters.safeGet(trimKey)
    val ignoreCaseParameter = classParameters.safeGet(ignoreCaseKey)
    (trimParameter, ignoreCaseParameter) match {
      case (InvalidParameterValueFound(errorMessage), _) =>
        unrecoverableErrorFor(trimKey,errorMessage)
      case (_, InvalidParameterValueFound(errorMessage)) =>
        unrecoverableErrorFor(trimKey,errorMessage)
      case (ValidParameterValueFound(shouldTrim), ValidParameterValueFound(shouldIgnoreCase)) =>
        val trim = if(shouldTrim) List(Trim) else Nil
        val ignoreCase = if(shouldIgnoreCase) List(ToUpperCase) else Nil
        Right(ValueMapper(trim ::: ignoreCase))
    }
  }


  private val defaultValueMapperInclusion = false
  private val trimKey = ParameterAttributeWithDefaultValue("trimValue",AttributeConversionFunctions.toBoolean, defaultValueMapperInclusion)
  private val ignoreCaseKey = ParameterAttributeWithDefaultValue("ignoreValueCase",AttributeConversionFunctions.toBoolean, defaultValueMapperInclusion)
  private val valueMapperAttributeKeys = List(trimKey, ignoreCaseKey)

  private val whileDoing = "creating comparison value transformation"
  private def message(key: String) = s"invalid value in attribute $key"
  private def preventativeAction(key: String) = s"provide a valid value for attribute $key"

  private def unrecoverableErrorFor[T](parameterAttribute: ParameterAttribute[T], errorMessage: ErrorMessage): Either[ErrorMessage, ValueMapper] =
  Left(
    UnrecoverableError(
      whileDoing,
      message(parameterAttribute.key),
      preventativeAction(parameterAttribute.key),
      List(errorMessage)
    ))
}