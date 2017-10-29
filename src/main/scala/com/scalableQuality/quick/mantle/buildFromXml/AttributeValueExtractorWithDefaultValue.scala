package com.scalableQuality.quick.mantle.buildFromXml

import com.scalableQuality.quick.mantle.log.ErrorMessage

import scala.xml.Attribute

class AttributeValueExtractorWithDefaultValue[ValueType](
                                                     key: String,
                                                     attributeConverter: Attribute => Either[ErrorMessage,ValueType],
                                                     defaultValue: ValueType
                                                   )
  extends AttributeValueExtractor[ValueType](
    key,
    attributeConverter
  ){
  override def notFound: ValidParameterValueFound[ValueType] = ValidParameterValueFound(defaultValue)
}


object AttributeValueExtractorWithDefaultValue {
  def apply[ValueType](
                        key: String,
                        attributeConverter: Attribute => Either[ErrorMessage,ValueType],
                        defaultValue: ValueType
                      ): AttributeValueExtractorWithDefaultValue[ValueType] = new AttributeValueExtractorWithDefaultValue(
    key,
    attributeConverter,
    defaultValue
  )
}