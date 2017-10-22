package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.others.{ColumnUsageStages, ShouldUseDuring, ValueMapper}
import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.MetaData

class DelimitedColumnDescription(
                                  metaData: ColumnDescriptionMetaData,
                                  position: DelimitedPosition,
                                  comparisonMapper: ValueMapper
                                ) {

  def shouldUseDuring(stages: ColumnUsageStages*): Boolean = metaData.shouldUseDuring(stages:_*)
  def columnValue(row: Vector[String]): Option[String] = position.extractColumnValue(row)
  def comparisonValue(row: Vector[String]): Option[String] = comparisonMapper(columnValue(row))
  def compareTwoColumns(leftRow: Option[Vector[String]], rightRow: Option[Vector[String]]): ComparisonBetweenTwoColumns =
    ComparisonBetweenTwoColumns(
      this.metaData,
      leftRow.flatMap(this.columnValue),
      rightRow.flatMap(this.columnValue),
      compare(leftRow, rightRow)
    )
  private def compare(leftRow: Option[Vector[String]], rightRow: Option[Vector[String]]): Boolean = {
    val leftColumn = leftRow.flatMap(this.comparisonValue)
    val rightColumn = rightRow.flatMap(this.comparisonValue)
    leftColumn == rightColumn
  }
}

object DelimitedColumnDescription {
  def apply(
             metaData: ColumnDescriptionMetaData,
             position: DelimitedPosition,
             comparisonMapper: ValueMapper
           ): DelimitedColumnDescription = new DelimitedColumnDescription(metaData, position, comparisonMapper)

  def apply(metaData: MetaData): Either[ErrorMessage, DelimitedColumnDescription] = {
    val classParameters = XMLAttributesToClassParameters(metaData,labelKey)
    val labelAttribute = classParameters.get(labelKey)

    val delimitedPositionEither = DelimitedPosition(metaData)
    val shouldUseDuringAttribute = ShouldUseDuring(metaData)
    val comparisonMapperAttribute = ValueMapper(metaData)

    (labelAttribute, delimitedPositionEither, shouldUseDuringAttribute, comparisonMapperAttribute) match {
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
        Right(DelimitedColumnDescription(metaData, position, comparisonMapper))
    }
  }
  private def connotMake(errorMessage: ErrorMessage): Either[ErrorMessage, DelimitedColumnDescription] = Left(
    UnrecoverableError(
      "creating a column description",
      "cannot create a column with invalid parameters",
      "fix the problems mentioned below",
      List(errorMessage)
    )
  )
  private val labelKey = ParameterAttribute("label", AttributeConversionFunctions.extractValue)
}
