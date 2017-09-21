package com.scalableQuality.quick.mantle.buildFromXml


import scala.annotation.tailrec
import scala.xml.{Attribute, MetaData, Null}
import scala.collection.mutable

class XMLAttributesToClassParameters(
                                      parametersMap: Map[String, Attribute]
                                    ) {
  def get[ValueType](parameterAttribute: ParameterAttribute[ValueType]): ParameterValue[ValueType] = parametersMap.get(parameterAttribute.key) match {
    case Some(attribute) => parameterAttribute.valueFrom(attribute)
    case None =>
      parameterAttribute.notFound
  }

  def safeGet[ValueType](
                          parameterAttributeWithDefaultValue: ParameterAttributeWithDefaultValue[ValueType]
                        ): ParameterValueFound[ValueType] = parametersMap.get(parameterAttributeWithDefaultValue.key) match {
    case Some(attribute) => parameterAttributeWithDefaultValue.valueFrom(attribute)
    case None => parameterAttributeWithDefaultValue.notFound
  }

  def toList: List[(String, Attribute)] = parametersMap.toList
}

object XMLAttributesToClassParameters {
  def apply(
             xmlMetaData: MetaData,
             attributeKeys: List[ParameterAttribute[_]]
           ): XMLAttributesToClassParameters = {
    @tailrec def loop(
                       metaDataList: MetaData,
                       attributeKeys: List[ParameterAttribute[_]],
                       parametersMap: mutable.HashMap[String, Attribute]
                     ): Map[String, Attribute] = metaDataList match {
      case Null =>
        parametersMap.toMap
      case attribute: Attribute =>
        attributeKeys.find(_ matches attribute) match {
          case None =>
            loop(
              attribute.next,
              attributeKeys,
              parametersMap
            )
          case Some(attributeKey) =>
            loop(
              attribute.next,
              attributeKeys diff List(attributeKey),
              parametersMap += (attributeKey.key -> attribute)
            )
        }
      case _ =>
        loop(
          metaDataList.next,
          attributeKeys,
          parametersMap
        )
    }

    val parametersMap = loop(xmlMetaData, attributeKeys, mutable.HashMap[String, Attribute]())
    new XMLAttributesToClassParameters(parametersMap)
  }
  def apply(
             xmlMetaData: MetaData,
             attributeKeys: ParameterAttribute[_]*
           ): XMLAttributesToClassParameters = XMLAttributesToClassParameters(xmlMetaData, attributeKeys.toList)
}
