package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts.{FixedColumnDescription, OrderedRowDescription}
import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.annotation.tailrec
import scala.xml.{Elem, Node}

class RowIdentifier(
                     columnIdentifiers: List[ColumnIdentifier],
                     val orderedRowDescription: OrderedRowDescription
                   ) {

  def canIdentify(rawRow: RawRow): Boolean = columnIdentifiers match {
    case Nil => RowIdentifier.defaultIdentificationResult
    case columnIdentifier::restOfColumnIdentifiers =>
      val canIdentifyTheFirstColumn = columnIdentifier(rawRow)
      restOfColumnIdentifiers.foldLeft(canIdentifyTheFirstColumn)(_ && _(rawRow))
  }
}

object RowIdentifier {
  def apply(
             columnIdentifiers: List[ColumnIdentifier],
             orderedRowDescription: OrderedRowDescription
           ): Either[ErrorMessage, RowIdentifier] = columnIdentifiers match {
    case Nil =>
      val errorMessage = UnrecoverableError(
        "creating a row identifier",
        "cannot make a row identifier without column identifiers",
        "add a column identifier"
      )
      Left(errorMessage)
    case _ =>
      val rowIdentifier = new RowIdentifier(columnIdentifiers, orderedRowDescription)
      Right(rowIdentifier)
  }

  def apply(rowDescriptionXmlElem : Elem): Either[ErrorMessage, RowIdentifier] = {

    @tailrec def makeColumnIdentifiersAndColumnDescriptions (
                                                              xmlNodes: List[Node],
                                                              columnDescriptionList: List[FixedColumnDescription],
                                                              columnIdentifierList: List[ColumnIdentifier]
                                                           ):
    Either[ErrorMessage, (List[FixedColumnDescription],List[ColumnIdentifier])] = xmlNodes match {
      case Nil =>
        Right((columnDescriptionList, columnIdentifierList))
      case (elem: Elem)::restOfNodes if compareXmlElemLabelsWith(elem,RowIdentifier.columnDescriptionElemLabel) =>
        val columnDescriptionEither = FixedColumnDescription(elem.attributes)
        columnDescriptionEither match {
          case Left(errorMessage) =>
            createErrorMessage(errorMessage)

          case Right(columnDescription) =>
            makeColumnIdentifiersAndColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifierList)
        }

      case (elem: Elem)::restOfNodes if compareXmlElemLabelsWith(elem,RowIdentifier.columnIdentifierElemLabel) =>
        val columnIdentifierEither = ColumnIdentifier(elem.attributes)
        columnIdentifierEither match {
          case Left(errorMessage) =>
            createErrorMessage(errorMessage)

          case Right((columnDescription, columnIdentifier)) =>
            makeColumnIdentifiersAndColumnDescriptions(restOfNodes, columnDescription :: columnDescriptionList, columnIdentifier :: columnIdentifierList)
        }

      case (elem: Elem):: _ =>
        val errorMessage = UnrecoverableError (
          "creating a column description",
          s"${elem.label} is not supported",
          "use a supported elem"
        )
        createErrorMessage(errorMessage)

      case _::restOfNodes =>
        makeColumnIdentifiersAndColumnDescriptions(restOfNodes, columnDescriptionList, columnIdentifierList)
    }
    val columnIdentifiersAndColumnDescriptions = makeColumnIdentifiersAndColumnDescriptions(rowDescriptionXmlElem.child.toList, Nil, Nil)

    val classParametersFromXmlAttributes = XMLAttributesToClassParameters(rowDescriptionXmlElem.attributes,RowIdentifier.orderedRowDescriptionLabelAttributeKey)
    val rowDescriptionLabelParameterValue = classParametersFromXmlAttributes.get(RowIdentifier.orderedRowDescriptionLabelAttributeKey)

    (rowDescriptionLabelParameterValue, columnIdentifiersAndColumnDescriptions) match {
      case (parameterValueError : ParameterValueError[_], _) =>
        createErrorMessage(parameterValueError.errorMessage)
      case (_, Left(errorMessage)) =>
        createErrorMessage(errorMessage)
      case (ValidParameterValueFound(label), Right((columnDescriptionList, columnIdentifierList))) =>
        val orderedRowDescription = OrderedRowDescription(columnDescriptionList, label)
        RowIdentifier(columnIdentifierList, orderedRowDescription )
    }
  }

  private [RowIdentifier] val defaultIdentificationResult = false

  private val columnIdentifierElemLabel = "columnIdentifier"
  private val columnDescriptionElemLabel = "columnDescription"

  private val orderedRowDescriptionLabelAttributeKey = ParameterAttribute("label", AttributeConversionFunctions.extractValue)

  private def compareXmlElemLabelsWith(elem: Elem, label: String):Boolean = elem.label.toLowerCase == label.toLowerCase()
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
































