package com.scalableQuality.quick.mantle.constructFromXml

import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.xml.{Attribute, MetaData}

class AttributesValuesExtractor(
                                 attributesMap: Map[AttributeValueExtractor[_], Attribute]
                               ) {
  def get[ValueType](
                      attributeValueExtractor: AttributeValueExtractor[ValueType]
                    ): Either[UnrecoverableError, ValueType] = attributesMap.get(attributeValueExtractor) match {
    case Some(attribute) =>
      attributeValueExtractor.valueFrom(attribute)

    case None =>
      attributeValueExtractor.default
  }
}


object AttributesValuesExtractor {

  def apply(
             attributesMap: Map[AttributeValueExtractor[_], Attribute]
           ): AttributesValuesExtractor = new AttributesValuesExtractor(attributesMap)


  def apply(
           metaData: MetaData,
           attributesValueExtractor: AttributeValueExtractor[_],
           restOfAttributesValuesExtractors: AttributeValueExtractor[_]*
           ): AttributesValuesExtractor =
    AttributesValuesExtractor(
      metaData,
      attributesValueExtractor :: restOfAttributesValuesExtractors.toList
    )

  def apply(
             metaData: MetaData,
             attributesValuesExtractors : List[AttributeValueExtractor[_]]
           ): AttributesValuesExtractor = {

    @tailrec def loop(
                       attributes: List[Attribute],
                       attributesValuesExtractors : List[AttributeValueExtractor[_]],
                       attributeMap: mutable.HashMap[AttributeValueExtractor[_], Attribute]
                     ): immutable.Map[AttributeValueExtractor[_], Attribute] = attributes match {
      case Nil =>
        attributeMap.toMap
      case attribute::restOfAttributes =>
        val attributeValueExtractorOpt = attributesValuesExtractors.find(_ matches(attribute))
        attributeValueExtractorOpt match {
          case None =>
            loop(
              restOfAttributes,
              attributesValuesExtractors,
              attributeMap
            )
          case Some(attributeValueExtractor) =>
            loop(
              restOfAttributes,
              attributesValuesExtractors diff List(attributeValueExtractor),
              attributeMap += (attributeValueExtractor -> attribute)
            )
        }
    }
    val attributes = XMLHelperFunctions.attributesList(metaData)
    val attributesMap = loop(
      attributes,
      attributesValuesExtractors,
      mutable.HashMap[AttributeValueExtractor[_], Attribute]()
    )
    AttributesValuesExtractor(attributesMap)
  }
}