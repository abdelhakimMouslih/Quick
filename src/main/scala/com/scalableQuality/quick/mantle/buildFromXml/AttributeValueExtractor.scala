package com.scalableQuality.quick.mantle.buildFromXml

import com.scalableQuality.quick.mantle.constructFromXml.XMLHelperFunctions
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.Attribute

class AttributeValueExtractor[ValueType](
                                     val key: String,
                                     attributeConverter: Attribute => Either[ErrorMessage,ValueType]
                                   ) {
  def valueFrom(attribute: Attribute): ParameterValueFound[ValueType] =
    ParameterValueFound(attributeConverter(attribute))

  def matches(attribute: Attribute): Boolean = XMLHelperFunctions.haveKey(attribute, this.key)

  def notFound: ParameterValue[ValueType] =  ParameterValueNotFound(
    UnrecoverableError(
      s"extracting the ${this.key} attribute",
      s"could not find the ${this.key} attribute",
      s"supply a ${this.key} attribute"
    )
  )
}

object AttributeValueExtractor{
  def apply[ValueType](
             key: String,
             AttributeConverter: Attribute => Either[ErrorMessage,ValueType]
  ): AttributeValueExtractor[ValueType] = new AttributeValueExtractor[ValueType](key, AttributeConverter)
}