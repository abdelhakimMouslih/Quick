package com.scalableQuality.quick.mantle.buildFromXml

import com.scalableQuality.quick.mantle.log.ErrorMessage

import scala.xml.Attribute

class ParameterAttributeWithDefaultValue[ValueType](
                                                     key: String,
                                                     attributeConverter: Attribute => Either[ErrorMessage,ValueType],
                                                     defaultValue: ValueType
                                                   )
  extends ParameterAttribute[ValueType](
    key,
    attributeConverter
  ){
  override def notFound: ValidParameterValueFound[ValueType] = ValidParameterValueFound(defaultValue)
}


object ParameterAttributeWithDefaultValue {
  def apply[ValueType](
                        key: String,
                        attributeConverter: Attribute => Either[ErrorMessage,ValueType],
                        defaultValue: ValueType
                      ): ParameterAttributeWithDefaultValue[ValueType] = new ParameterAttributeWithDefaultValue(
    key,
    attributeConverter,
    defaultValue
  )
}