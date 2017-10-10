package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.others.{ShouldUseDuring, ValueMapper}
import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.xml.MetaData


case class FixedColumnDescription(
                                   val metaData: ColumnDescriptionMetaData,
                                   val position: ColumnPosition,
                                   comparisonMapper: ValueMapper
                               ) {
  def columnValue(row: RawRow): Option[String] = position.extractColumnValue(row)
  def comparisonValue(row: RawRow): Option[String] = comparisonMapper(columnValue(row))
  def compareTwoColumns(leftRow: Option[RawRow], rightRow: Option[RawRow]): ComparisonBetweenTwoColumns =
    ComparisonBetweenTwoColumns(
      this.metaData,
      leftRow.flatMap(this.columnValue),
      rightRow.flatMap(this.columnValue),
      compare(leftRow, rightRow)
    )
  private def compare(leftRow: Option[RawRow], rightRow: Option[RawRow]): Boolean = {
    val leftColumn = leftRow.flatMap(this.comparisonValue)
    val rightColumn = rightRow.flatMap(this.comparisonValue)
    leftColumn == rightColumn
  }
}

object FixedColumnDescription {
  /*def apply(
             position: ColumnPosition,
             shouldUseDuring: ShouldUseDuring,
             comparisonMapper: ValueMapper,
             label: String
           ): FixedColumnDescription = new FixedColumnDescription(position, shouldUseDuring, comparisonMapper, label)

*/

  def apply(metaData: MetaData): Either[ErrorMessage, FixedColumnDescription] = {
    val classParameters = XMLAttributesToClassParameters(metaData,ColumnDescriptionAttributeKeys)
    val labelAttribute = classParameters.get(labelKey)
    val positionAttribute = ColumnPosition(metaData)
    val shouldUseDuringAttribute = ShouldUseDuring(metaData)
    val comparisonMapperAttribute = ValueMapper(metaData)
    (labelAttribute, positionAttribute, shouldUseDuringAttribute, comparisonMapperAttribute) match {
      case (paramError: ParameterValueError[_],_,_,_) =>
        connotMake(paramError.errorMessage)
      case (_,Left(errorMessage),_,_) =>
        connotMake(errorMessage)
      case (_,_,Left(errorMessage),_) =>
        connotMake(errorMessage)
      case (_,_,_,Left(errorMessage)) =>
        connotMake(errorMessage)
      case (ValidParameterValueFound(label),Right(position),Right(shouldUseDuring),Right(comparisonMapper)) =>
        val metaData = ColumnDescriptionMetaData(position.toString, label, shouldUseDuring)
        Right(FixedColumnDescription(metaData, position, comparisonMapper))
    }
  }
  private def connotMake(errorMessage: ErrorMessage): Either[ErrorMessage, FixedColumnDescription] = Left(
    UnrecoverableError(
      "creating a column description",
      "cannot create a column with invalid parameters",
      "fix the problems mentioned below",
      List(errorMessage)
    )
  )
  private val labelKey = ParameterAttribute("label", AttributeConversionFunctions.extractValue)
  private val ColumnDescriptionAttributeKeys = List(labelKey)
}
