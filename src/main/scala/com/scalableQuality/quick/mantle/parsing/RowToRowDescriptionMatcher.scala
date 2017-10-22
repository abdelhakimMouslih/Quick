package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts._
import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.annotation.tailrec
import scala.xml.{Elem, Node}

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

  def apply(rowDescriptionXmlElem : Elem): Either[ErrorMessage, RowToRowDescriptionMatcher] =
    if (compareXmlElemLabelsWith(rowDescriptionXmlElem, fixedOrderedRowDescriptionElemLabel)) {
      makeFixedRowToRowDescriptionMatcher(rowDescriptionXmlElem)
    } else if(compareXmlElemLabelsWith(rowDescriptionXmlElem, delimitedOrderedRowDescriptionElemLabel)){
      makeDelimitedRowToRowDescriptionMatcher(rowDescriptionXmlElem)
    } else {
      val errorMessage = UnrecoverableError(
        "making RowToRowDescriptionMatcher",
        s"${rowDescriptionXmlElem.label} is not supported",
        "use either OrderedRowDescription"
      )
      Left(errorMessage)
    }


  def makeDelimitedRowToRowDescriptionMatcher(rowDescriptionXmlElem : Elem):Either[ErrorMessage, RowToRowDescriptionMatcher] = {

    @tailrec def makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(
                                                                       xmlNodes: List[Elem],
                                                                       columnDescriptionList: List[DelimitedColumnDescription],
                                                                       columnIdentifierList: List[DelimitedColumnIdentifier]
                                                                     ):
    Either[ErrorMessage, (List[DelimitedColumnDescription],List[DelimitedColumnIdentifier])] = xmlNodes match {
      case elem :: _
        if ! compareXmlElemLabelsWith(
          elem, RowToRowDescriptionMatcher.columnDescriptionElemLabel,
          RowToRowDescriptionMatcher.columnIdentifierElemLabel
        ) =>
        val errorMessage = UnrecoverableError (
          "creating a column description",
          s"${elem.label} is not supported",
          "use a BOBO elem"
        )
        createErrorMessage(errorMessage)

      case Nil =>
        Right((columnDescriptionList, columnIdentifierList))

      case elem::restOfNodes if compareXmlElemLabelsWith(elem,RowToRowDescriptionMatcher.columnDescriptionElemLabel) =>
        val columnDescriptionEither = DelimitedColumnDescription(elem.attributes)
        columnDescriptionEither match {
          case Left(errorMessage) =>
            createErrorMessage(errorMessage)

          case Right(columnDescription) =>
            makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifierList)
        }

      case elem::restOfNodes if compareXmlElemLabelsWith(elem,RowToRowDescriptionMatcher.columnIdentifierElemLabel) =>
        val columnIdentifierEither = DelimitedColumnIdentifier(elem.attributes)
        columnIdentifierEither match {
          case Left(errorMessage) =>
            createErrorMessage(errorMessage)

          case Right((columnDescription, columnIdentifier)) =>
            makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifier :: columnIdentifierList)
        }

    }


    val classParametersFromXmlAttributes  = XMLAttributesToClassParameters(
      rowDescriptionXmlElem.attributes,
      orderedRowDescriptionLabelAttributeKey,
      orderedRowDescriptionDelimiterAttributeKey)
    val labelClassParameter = classParametersFromXmlAttributes.get(orderedRowDescriptionLabelAttributeKey)

    val delimiterClassParameter: Either[ErrorMessage, LiteralDelimiter] = classParametersFromXmlAttributes.get(orderedRowDescriptionDelimiterAttributeKey) match {
      case parameterValueError : ParameterValueError[_] =>
        val errorMessage = UnrecoverableError(
          "making stuff",
          "stuff cannot be make",
          s"fee me, ${parameterValueError.errorMessage}"
        )
        Left(errorMessage)
      case ValidParameterValueFound(delimiterValue) =>
        LiteralDelimiter(delimiterValue)
    }

    val rowDescriptionElemChildren = collectAllChildElems(rowDescriptionXmlElem)
    val delimitedColumnDescriptionsAndIdentifiers = makeDelimitedColumnIdentifiersAndDelimitedColumnDescriptions(
      rowDescriptionElemChildren,
      Nil,
      Nil
    )
    (labelClassParameter, delimiterClassParameter, delimitedColumnDescriptionsAndIdentifiers ) match {
      case (parameterValueError : ParameterValueError[_] ,_,_) =>
        val errorMessage = UnrecoverableError(
          "making stuff",
          "stuff cannot be make",
          s"fee me, ${parameterValueError.errorMessage}"
        )
        Left(errorMessage)
      case (_,Left(errorMessage),_) =>
        Left(errorMessage)
      case (_,_,Left(errorMessage)) =>
        Left(errorMessage)
      case (ValidParameterValueFound(label),Right(delimiter),Right((delimitedColumnsDescriptionsList,delimitedColumnsIdentifiersList))) =>
        val delimitedRowIdentifierEither = DelimitedRowIdentifier(delimitedColumnsIdentifiersList, delimiter)
        delimitedRowIdentifierEither match {
          case Left(errorMessage) =>
            Left(errorMessage)
          case Right(delimitedRowIdentifier) =>
            val delimitedRowDivider = DelimitedRowDivider(delimitedColumnsDescriptionsList, delimiter)
            val delimitedRowDescriptions = OrderedRowDescription(delimitedRowDivider, label)
            val rowToRowDescriptionMatcher = RowToRowDescriptionMatcher(delimitedRowIdentifier, delimitedRowDescriptions)
            Right(rowToRowDescriptionMatcher)
        }
    }


  }


    def makeFixedRowToRowDescriptionMatcher(rowDescriptionXmlElem : Elem): Either[ErrorMessage, RowToRowDescriptionMatcher] = {

    @tailrec def makeFixedColumnIdentifiersAndFixedColumnDescriptions(
                                                              xmlNodes: List[Elem],
                                                              columnDescriptionList: List[FixedColumnDescription],
                                                              columnIdentifierList: List[FixedColumnIdentifier]
                                                           ):
    Either[ErrorMessage, (List[FixedColumnDescription],List[FixedColumnIdentifier])] = xmlNodes match {
      case elem :: _
        if ! compareXmlElemLabelsWith(
          elem, RowToRowDescriptionMatcher.columnDescriptionElemLabel,
          RowToRowDescriptionMatcher.columnIdentifierElemLabel
        ) =>
        val errorMessage = UnrecoverableError (
          "creating a column description",
          s"${elem.label} is not supported",
          "use a BOBO elem"
        )
        createErrorMessage(errorMessage)

      case Nil =>
        Right((columnDescriptionList, columnIdentifierList))

      case elem::restOfNodes if compareXmlElemLabelsWith(elem,RowToRowDescriptionMatcher.columnDescriptionElemLabel) =>
        val columnDescriptionEither = FixedColumnDescription(elem.attributes)
        columnDescriptionEither match {
          case Left(errorMessage) =>
            createErrorMessage(errorMessage)

          case Right(columnDescription) =>
            makeFixedColumnIdentifiersAndFixedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifierList)
        }

      case elem::restOfNodes if compareXmlElemLabelsWith(elem,RowToRowDescriptionMatcher.columnIdentifierElemLabel) =>
        val columnIdentifierEither = FixedColumnIdentifier(elem.attributes)
        columnIdentifierEither match {
          case Left(errorMessage) =>
            createErrorMessage(errorMessage)

          case Right((columnDescription, columnIdentifier)) =>
            makeFixedColumnIdentifiersAndFixedColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifier :: columnIdentifierList)
        }

    }


    val rowDescriptionXmlElemChildrenList = collectAllChildElems(rowDescriptionXmlElem)
    val columnIdentifiersAndColumnDescriptions = makeFixedColumnIdentifiersAndFixedColumnDescriptions(rowDescriptionXmlElemChildrenList, Nil, Nil)

    val classParametersFromXmlAttributes = XMLAttributesToClassParameters(rowDescriptionXmlElem.attributes,RowToRowDescriptionMatcher.orderedRowDescriptionLabelAttributeKey)
    val rowDescriptionLabelParameterValue = classParametersFromXmlAttributes.get(RowToRowDescriptionMatcher.orderedRowDescriptionLabelAttributeKey)

    (rowDescriptionLabelParameterValue, columnIdentifiersAndColumnDescriptions) match {
      case (parameterValueError : ParameterValueError[_], _) =>
        createErrorMessage(parameterValueError.errorMessage)
      case (_, Left(errorMessage)) =>
        createErrorMessage(errorMessage)
      case (ValidParameterValueFound(label), Right((columnDescriptionList, columnIdentifierList))) =>
        val rowIdentifierEither = FixedRowIdentifier(columnIdentifierList)
        rowIdentifierEither match {
          case Left(error) =>
            Left(error)
          case Right(rowIdentifier) =>
            val fixedRowDivider = FixedRowDivider(columnDescriptionList)
            val orderedRowDescription = OrderedRowDescription(fixedRowDivider, label)
            val rowToRowDescriptionMatcher = RowToRowDescriptionMatcher(rowIdentifier, orderedRowDescription )
            Right(rowToRowDescriptionMatcher)
        }
    }
  }

  val defaultIdentificationResult = false

  private def collectAllChildElems(node: Node): List[Elem] = node.child.toList.collect{
    case elem: Elem => elem
  }


  private val delimitedOrderedRowDescriptionElemLabel = "DelimitedOrderedRowDescription"
  private val fixedOrderedRowDescriptionElemLabel = "FixedOrderedRowDescription"
  private val columnIdentifierElemLabel = "columnIdentifier"
  private val columnDescriptionElemLabel = "columnDescription"

  private val orderedRowDescriptionLabelAttributeKey = ParameterAttribute("label", AttributeConversionFunctions.extractValue)
  private val orderedRowDescriptionDelimiterAttributeKey = ParameterAttribute("literalDelimiter", AttributeConversionFunctions.extractValue)

  private def compareXmlElemLabelsWith(elem: Elem, firstLabel: String, restOfLabels: String*):Boolean = {
    val ElemLabel = elem.label.toLowerCase
    val firstResult = ElemLabel == firstLabel.toLowerCase
    restOfLabels.foldLeft(firstResult){
      (previousResult, label) => previousResult || ElemLabel == label.toLowerCase
    }
  }

  private def createErrorMessage[T](childErrorMessage: ErrorMessage* ) : Either[ErrorMessage, T] = {
    val errorMessage = UnrecoverableError(
      "creating a column description",
      "encountered a problem",
      "solve the problem below",
      childErrorMessage.toList
    )
    Left(errorMessage)
  }

}
































