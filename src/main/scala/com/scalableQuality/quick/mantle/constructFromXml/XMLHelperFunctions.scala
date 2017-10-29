package com.scalableQuality.quick.mantle.constructFromXml

import scala.annotation.tailrec
import scala.xml._

object XMLHelperFunctions {

  def haveKey(attribute: Attribute, key: String ) : Boolean = attribute.key.equalsIgnoreCase(key.toLowerCase)

  def haveLabel(elem: Elem, label: String): Boolean = elem.label.equalsIgnoreCase(label)

  def collectElemChildren(node: Node): List[Elem] =  node.child.toList.collect{
    case elem: Elem => elem
  }

  def attributesList(metaData: MetaData): List[Attribute] = {
    @tailrec def loop(metaData: MetaData, attributes: List[Attribute]) : List[Attribute] =
    metaData match {
      case Null =>
        attributes
      case attribute: Attribute =>
        loop(
          attribute.next,
          attribute :: attributes
        )
      case _ =>
        loop(
          metaData.next,
          attributes
        )
    }
    loop(metaData, Nil)
  }

  def elemLabelIsIn(elem: Elem, firstLabel: String, restOfLabels: String*):Boolean = {
    val firstResult = haveLabel(elem, firstLabel)
    restOfLabels.foldLeft(firstResult){
      (previousResult, label) => previousResult || haveLabel(elem, label)
    }
  }
}
