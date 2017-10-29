package com.scalableQuality.quick.mantle.constructFromXml

import com.scalableQuality.quick.mantle.constructFromXml.errorMessages.XMLHelperFunctionsErrorMessages
import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.annotation.tailrec
import scala.xml._

object XMLHelperFunctions {

  def haveKey(attribute: Attribute, key: String ) : Boolean = attribute.key.equalsIgnoreCase(key.toLowerCase)

  def haveLabel(elem: Elem, label: String): Boolean = elem.label.equalsIgnoreCase(label)

  def collectElemChildren(node: Node): List[Elem] =  node.child.toList.collect{
    case elem: Elem => elem
  }

  def attributesList(metaData: MetaData): List[Attribute] = {
    metaDataFlatten(metaData).collect {
      case attribute: Attribute => attribute
    }
  }

  def metaDataFlatten(metaData: MetaData) : List[MetaData] = metaData.toList.map(_.copy(Null))

  def metaDataJoin(metaDataList: List[MetaData]): MetaData = metaDataList.foldRight(Null: MetaData){
    (metaData, accumulator) => metaData.copy(accumulator)
  }

  def removeAttributes(metaData: MetaData, attributeValueExtractorList: List[AttributeValueExtractor[_]]) =
    attributeValueExtractorList.foldLeft(metaData){
      (currentMetaData: MetaData, valueExtractor: AttributeValueExtractor[_]) => currentMetaData.remove(valueExtractor.key)
    }

  def elemLabelIsIn(elem: Elem, firstLabel: String, restOfLabels: String*):Boolean = {
    val firstResult = haveLabel(elem, firstLabel)
    restOfLabels.foldLeft(firstResult){
      (previousResult, label) => previousResult || haveLabel(elem, label)
    }
  }

  def collectUnknownAttributes(attributesValuesExtractors: List[AttributeValueExtractor[_]], metaData: MetaData): List[UnrecoverableError] = {
    @tailrec def loop(
                     attributeKeys: List[AttributeValueExtractor[_]],
                     attributes: List[Attribute],
                     unknownAttributeErrors: List[UnrecoverableError]
                     ): List[UnrecoverableError] = attributes match {
      case Nil =>
          unknownAttributeErrors
      case attribute::restOfAttributes =>
        val attributeKeyOpt = attributeKeys.find(_ matches(attribute))
        attributeKeyOpt match {
          case None =>
            val errorMessage = XMLHelperFunctionsErrorMessages.unknownAttributeError(attribute)
            loop(
              attributeKeys,
              restOfAttributes,
              errorMessage :: unknownAttributeErrors
            )
          case Some(attributeKey) =>
            loop(
              attributeKeys diff List(attributeKey),
              restOfAttributes,
              unknownAttributeErrors
            )
        }
    }
    val attributeList = attributesList(metaData)
    loop(attributesValuesExtractors, attributeList, Nil)
  }

  /*def partition(metaData:MetaData, condition : MetaData => Boolean): (MetaData, MetaData) = {
    val metaDataList =
  }*/

}
