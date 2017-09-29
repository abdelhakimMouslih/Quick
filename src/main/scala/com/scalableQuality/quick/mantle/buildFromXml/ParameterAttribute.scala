package com.scalableQuality.quick.mantle.buildFromXml

import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.Attribute

class ParameterAttribute[ValueType](
                                     val key: String,
                                     attributeConverter: Attribute => Either[ErrorMessage,ValueType]
                                   ) {
  def valueFrom(attribute: Attribute): ParameterValueFound[ValueType] =
    ParameterValueFound(attributeConverter(attribute))

  def matches(attribute: Attribute): Boolean = attribute.key.toLowerCase == this.key.toLowerCase

  def notFound: ParameterValue[ValueType] =  ParameterValueNotFound(
    UnrecoverableError(
      s"extracting the ${this.key} attribute",
      s"could not find the ${this.key} attribute",
      s"supply a ${this.key} attribute"
    )
  )
}

object ParameterAttribute{
  def apply[ValueType](
             key: String,
             AttributeConverter: Attribute => Either[ErrorMessage,ValueType]
  ): ParameterAttribute[ValueType] = new ParameterAttribute[ValueType](key, AttributeConverter)
}