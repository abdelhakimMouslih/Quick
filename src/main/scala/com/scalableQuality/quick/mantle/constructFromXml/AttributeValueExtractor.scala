package com.scalableQuality.quick.mantle.constructFromXml

import com.scalableQuality.quick.mantle.error.{AttributeNotFoundError, EncounteredError, UnrecoverableError}

import scala.xml.Attribute

class AttributeValueExtractor[ValueType](
                                          val key: String,
                                          attributeConverter: Attribute => Either[UnrecoverableError,ValueType],
                                          val default: Either[UnrecoverableError,ValueType]
                                        ) {
  def valueFrom(attribute: Attribute): Either[UnrecoverableError,ValueType] = attributeConverter(attribute)
  def matches(attribute: Attribute): Boolean = XMLHelperFunctions.haveKey(attribute, this.key)
}

object AttributeValueExtractor{

  def apply[ValueType](
                        key: String,
                        AttributeConverter: Attribute => Either[UnrecoverableError,ValueType]
                      ): AttributeValueExtractor[ValueType] =
    new AttributeValueExtractor[ValueType](
      key,
      AttributeConverter,
      defaultToError(key)
    )

  def apply[ValueType](
                      key:String,
                      AttributeConverter: Attribute => Either[UnrecoverableError,ValueType],
                      default: Either[UnrecoverableError,ValueType]
                      ): AttributeValueExtractor[ValueType] =
    new AttributeValueExtractor[ValueType](
      key,
      AttributeConverter,
      default
    )

  private def defaultToError[ValueType](key: String): Either[UnrecoverableError,ValueType] =  {
    val errorMessage = AttributeNotFoundError(
      s"""extracting the value from attribute "${key}""",
      s"""could not find an attribute with key "${key}"""",
      s"""provide a valid attribute "${key}" """
    )
    Left[UnrecoverableError,ValueType](errorMessage)
  }
}