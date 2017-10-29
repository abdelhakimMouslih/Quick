package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts._
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.{BunchOfErrors, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RowToRowDescriptionMatcher.delimitedOrderedRowDescriptionAttributeList
import com.scalableQuality.quick.mantle.parsing.errorMessages.RowToRowDescriptionMatcherErrorMessages

import scala.annotation.tailrec
import scala.xml.{Attribute, Elem}

class RowToRowDescriptionMatcher(
                     rowIdentifier: RowIdentifier,
                     val orderedRowDescription: OrderedRowDescription
                   ) {
  def canIdentify(row: RawRow): Boolean = rowIdentifier.canIdentify(row)
}



object RowToRowDescriptionMatcher {

  def apply(
             rowIdentifier: RowIdentifier,
             orderedRowDescription: OrderedRowDescription
           ): RowToRowDescriptionMatcher =
    new RowToRowDescriptionMatcher(rowIdentifier, orderedRowDescription)

  def apply(rowDescriptionXmlElem : Elem): Either[UnrecoverableError, RowToRowDescriptionMatcher] =
    if (XMLHelperFunctions.haveLabel(rowDescriptionXmlElem, fixedOrderedRowDescriptionElemLabel)) {
      makeFixedRowToRowDescriptionMatcher(rowDescriptionXmlElem)
    } else if(XMLHelperFunctions.haveLabel(rowDescriptionXmlElem, delimitedOrderedRowDescriptionElemLabel)){
      makeDelimitedRowToRowDescriptionMatcher(rowDescriptionXmlElem)
    } else {
      RowToRowDescriptionMatcherErrorMessages.unknownRowDescriptionElem(rowDescriptionXmlElem)
    }


  def makeDelimitedRowToRowDescriptionMatcher(rowDescriptionXmlElem : Elem):Either[UnrecoverableError, RowToRowDescriptionMatcher] = {

    @tailrec def makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(
                                                                       xmlNodes: List[Elem],
                                                                       columnDescriptionList: List[DelimitedColumnDescription],
                                                                       columnIdentifierList: List[DelimitedColumnIdentifier]
                                                                     ):
    Either[UnrecoverableError, (List[DelimitedColumnDescription],List[DelimitedColumnIdentifier])] = xmlNodes match {
      case elem :: _
        if ! XMLHelperFunctions.elemLabelIsIn(
          elem, RowToRowDescriptionMatcher.columnDescriptionElemLabel,
          RowToRowDescriptionMatcher.columnIdentifierElemLabel
        ) =>
        RowToRowDescriptionMatcherErrorMessages.unknownDelimitedRowDescriptionChildElem(elem)

      case Nil =>
        Right((columnDescriptionList, columnIdentifierList))

      case elem::restOfNodes if XMLHelperFunctions.haveLabel(elem,RowToRowDescriptionMatcher.columnDescriptionElemLabel) =>
        val columnDescriptionEither = DelimitedColumnDescription(elem.attributes)
        columnDescriptionEither match {
          case Left(errorMessage) =>
            Left(errorMessage)

          case Right(columnDescription) =>
            makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifierList)
        }

      case elem::restOfNodes if XMLHelperFunctions.haveLabel(elem,RowToRowDescriptionMatcher.columnIdentifierElemLabel) =>
        val columnIdentifierEither = DelimitedColumnIdentifier(elem.attributes)
        columnIdentifierEither match {
          case Left(errorMessage) =>
            Left(errorMessage)

          case Right((columnDescription, columnIdentifier)) =>
            makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifier :: columnIdentifierList)
        }

    }

    val unknownAttributes = XMLHelperFunctions.collectUnknownAttributes(delimitedOrderedRowDescriptionAttributeList, rowDescriptionXmlElem.attributes)
    unknownAttributes match {
      case Nil =>
        val attributeValues  = AttributesValuesExtractor(
          rowDescriptionXmlElem.attributes,
          orderedRowDescriptionLabelAttributeKey,
          orderedRowDescriptionDelimiterAttributeKey)
        val labelAttributeValue = attributeValues.get(orderedRowDescriptionLabelAttributeKey)

        val delimiterAttributeValue = attributeValues.get(orderedRowDescriptionDelimiterAttributeKey)

        val rowDescriptionElemChildren = XMLHelperFunctions.collectElemChildren(rowDescriptionXmlElem)
        val delimitedColumnDescriptionsAndIdentifiers = makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(
          rowDescriptionElemChildren,
          Nil,
          Nil
        )

        (labelAttributeValue, delimiterAttributeValue, delimitedColumnDescriptionsAndIdentifiers ) match {
          case (Right(label),Right(delimiter),Right((delimitedColumnsDescriptionsList,delimitedColumnsIdentifiersList))) =>
            val delimitedRowIdentifierEither = DelimitedRowIdentifier(delimitedColumnsIdentifiersList, delimiter)
            delimitedRowIdentifierEither match {
              case Left(errorMessage) =>
                RowToRowDescriptionMatcherErrorMessages.invalidDelimitedRowDescriptionChildElemOrAttribute(errorMessage)
              case Right(delimitedRowIdentifier) =>
                val delimitedRowDivider = DelimitedRowDivider(delimitedColumnsDescriptionsList, delimiter)
                val delimitedRowDescriptions = OrderedRowDescription(delimitedRowDivider, label)
                val rowToRowDescriptionMatcher = RowToRowDescriptionMatcher(delimitedRowIdentifier, delimitedRowDescriptions)
                Right(rowToRowDescriptionMatcher)
            }
          case _ =>
            val errorMessages = UnrecoverableError.collectAllErrorsToList(labelAttributeValue, delimiterAttributeValue, delimitedColumnDescriptionsAndIdentifiers)
            RowToRowDescriptionMatcherErrorMessages.invalidDelimitedRowDescriptionChildElemOrAttribute(errorMessages:_*)
        }
      case _ =>
        RowToRowDescriptionMatcherErrorMessages.invalidDelimitedRowDescriptionChildElemOrAttribute(unknownAttributes:_*)
    }


  }


    def makeFixedRowToRowDescriptionMatcher(rowDescriptionXmlElem : Elem): Either[UnrecoverableError, RowToRowDescriptionMatcher] = {

    @tailrec def makeFixedColumnIdentifiersAndFixedColumnDescriptions(
                                                              xmlNodes: List[Elem],
                                                              columnDescriptionList: List[FixedColumnDescription],
                                                              columnIdentifierList: List[FixedColumnIdentifier]
                                                           ):
    Either[UnrecoverableError, (List[FixedColumnDescription],List[FixedColumnIdentifier])] = xmlNodes match {
      case elem :: _
        if ! XMLHelperFunctions.elemLabelIsIn(
          elem, RowToRowDescriptionMatcher.columnDescriptionElemLabel,
          RowToRowDescriptionMatcher.columnIdentifierElemLabel
        ) =>

        RowToRowDescriptionMatcherErrorMessages.unknownFixedRowDescriptionChildElem(elem)

      case Nil =>
        Right((columnDescriptionList, columnIdentifierList))

      case elem::restOfNodes if XMLHelperFunctions.haveLabel(elem,RowToRowDescriptionMatcher.columnDescriptionElemLabel) =>
        val columnDescriptionEither = FixedColumnDescription(elem.attributes)
        columnDescriptionEither match {
          case Left(errorMessage) =>
            Left(errorMessage)

          case Right(columnDescription) =>
            makeFixedColumnIdentifiersAndFixedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifierList)
        }

      case elem::restOfNodes if XMLHelperFunctions.haveLabel(elem,RowToRowDescriptionMatcher.columnIdentifierElemLabel) =>
        val columnIdentifierEither = FixedColumnIdentifier(elem.attributes)
        columnIdentifierEither match {
          case Left(errorMessage) =>
            Left(errorMessage)

          case Right((columnDescription, columnIdentifier)) =>
            makeFixedColumnIdentifiersAndFixedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifier :: columnIdentifierList)
        }

    }



      val unknownAttributes = XMLHelperFunctions.collectUnknownAttributes(fixedOrderedRowDescriptionAttributeList, rowDescriptionXmlElem.attributes)
      unknownAttributes match {
        case Nil =>
          val rowDescriptionXmlElemChildrenList = XMLHelperFunctions.collectElemChildren(rowDescriptionXmlElem)
          val columnIdentifiersAndColumnDescriptions = makeFixedColumnIdentifiersAndFixedColumnDescriptions(rowDescriptionXmlElemChildrenList, Nil, Nil)

          val AttributeValues = AttributesValuesExtractor(rowDescriptionXmlElem.attributes,RowToRowDescriptionMatcher.orderedRowDescriptionLabelAttributeKey)
          val rowDescriptionLabelAttributeValue = AttributeValues.get(RowToRowDescriptionMatcher.orderedRowDescriptionLabelAttributeKey)

          (rowDescriptionLabelAttributeValue, columnIdentifiersAndColumnDescriptions) match {
            case (Right(label), Right((columnDescriptionList, columnIdentifierList))) =>
              val rowIdentifierEither = FixedRowIdentifier(columnIdentifierList)
              rowIdentifierEither match {
                case Left(errorMessage) =>
                  RowToRowDescriptionMatcherErrorMessages.invalidFixedRowDescriptionChildElemOrAttribute(errorMessage)
                case Right(rowIdentifier) =>
                  val fixedRowDivider = FixedRowDivider(columnDescriptionList)
                  val orderedRowDescription = OrderedRowDescription(fixedRowDivider, label)
                  val rowToRowDescriptionMatcher = RowToRowDescriptionMatcher(rowIdentifier, orderedRowDescription )
                  Right(rowToRowDescriptionMatcher)
              }

            case _ =>
              val errorMessages = UnrecoverableError.collectAllErrorsToList(rowDescriptionLabelAttributeValue, columnIdentifiersAndColumnDescriptions)
              RowToRowDescriptionMatcherErrorMessages.invalidFixedRowDescriptionChildElemOrAttribute(errorMessages:_*)
          }

        case _ =>
          RowToRowDescriptionMatcherErrorMessages.invalidFixedRowDescriptionChildElemOrAttribute(unknownAttributes:_*)
      }

  }

  val defaultIdentificationResult = false

  private val delimitedOrderedRowDescriptionElemLabel = "DelimitedOrderedRowDescription"
  private val fixedOrderedRowDescriptionElemLabel = "FixedOrderedRowDescription"
  private val columnIdentifierElemLabel = "columnIdentifier"
  private val columnDescriptionElemLabel = "columnDescription"

  private val orderedRowDescriptionLabelAttributeKey = AttributeValueExtractor("label", AttributeValueConversion.extractValue)

  private def delimiterAttributeValueConverter(attribute: Attribute ) = AttributeValueConversion.extractValueAndConvertTo(LiteralDelimiter(_))(attribute)

  private val orderedRowDescriptionDelimiterAttributeKey = AttributeValueExtractor("literalDelimiter", RowToRowDescriptionMatcher.delimiterAttributeValueConverter(_))

  private val delimitedOrderedRowDescriptionAttributeList = List(orderedRowDescriptionLabelAttributeKey, orderedRowDescriptionDelimiterAttributeKey)
  private val fixedOrderedRowDescriptionAttributeList = List(orderedRowDescriptionLabelAttributeKey)
}
































