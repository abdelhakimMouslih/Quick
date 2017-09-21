package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.core.others.{ShouldUseDuring, ValueMapper}
import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.xml.MetaData


class ColumnDescription (
                               val position: ColumnPosition,
                               val shouldUseDuring: ShouldUseDuring,
                               comparisonMapper: ValueMapper,
                               val label: String
                               ) {
  def columnValue(row: RawRow): Option[String] = position.extractColumnValue(row)
  def comparisonValue(row: RawRow): Option[String] = comparisonMapper(columnValue(row))
}

object ColumnDescription {
  def apply(
             position: ColumnPosition,
             shouldUseDuring: ShouldUseDuring,
             comparisonMapper: ValueMapper,
             label: String
           ): ColumnDescription = new ColumnDescription(position, shouldUseDuring, comparisonMapper, label)



  def apply(metaData: MetaData): Either[ErrorMessage, ColumnDescription] = {
    val classParameters = XMLAttributesToClassParameters(metaData,ColumnDescriptionAttributeKeys)
    val labelAttribute = classParameters.get(labelKey)
    val positionAttribute = ColumnPosition(metaData)
    val shouldUseDuringAttribute = ShouldUseDuring(metaData)
    val comparisonMapperAttribute = ValueMapper.comparisonValueMapper(metaData)
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
        Right(ColumnDescription(position, shouldUseDuring, comparisonMapper,label))
    }
  }
  private def connotMake(errorMessage: ErrorMessage): Either[ErrorMessage, ColumnDescription] = Left(
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
