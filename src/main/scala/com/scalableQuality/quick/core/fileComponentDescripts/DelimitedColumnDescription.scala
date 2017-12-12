package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.fileComponentDescripts.errorMessages.DelimitedColumnDescriptionErrorMessages
import com.scalableQuality.quick.core.others.{
  ColumnUsageStages,
  ShouldUseDuring,
  ValueMapper
}
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  UnrecoverableError
}

import scala.xml.MetaData

class DelimitedColumnDescription(
    metaData: ColumnDescriptionMetaData,
    position: DelimitedPosition,
    comparisonMapper: ValueMapper
) {

  def shouldUseDuring(stages: ColumnUsageStages*): Boolean =
    metaData.shouldUseDuring(stages: _*)
  def columnValue(row: Vector[String]): Option[String] =
    position.extractColumnValue(row)
  def comparisonValue(row: Vector[String]): Option[String] =
    comparisonMapper(columnValue(row))
  def compareTwoColumns(
      leftRow: Option[Vector[String]],
      rightRow: Option[Vector[String]]): ComparisonBetweenTwoColumns =
    ComparisonBetweenTwoColumns(
      this.metaData,
      leftRow.flatMap(this.columnValue),
      rightRow.flatMap(this.columnValue),
      compare(leftRow, rightRow)
    )
  private def compare(leftRow: Option[Vector[String]],
                      rightRow: Option[Vector[String]]): Boolean = {
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
  ): DelimitedColumnDescription =
    new DelimitedColumnDescription(metaData, position, comparisonMapper)

  def apply(metaData: MetaData)
    : Either[UnrecoverableError, DelimitedColumnDescription] = {
    val unknownAttributeList = XMLHelperFunctions.collectUnknownAttributes(
      listOfAttributesKeys,
      metaData)
    unknownAttributeList match {
      case Nil =>
        val attributesValues = AttributesValuesExtractor(metaData, labelKey)
        val labelAttributeValue = attributesValues.get(labelKey)

        val delimitedPositionEither = DelimitedPosition(metaData)
        val shouldUseDuringAttribute = ShouldUseDuring(metaData)
        val comparisonMapperAttribute = ValueMapper(metaData)

        validateAttributeValues(labelAttributeValue,
                                delimitedPositionEither,
                                shouldUseDuringAttribute,
                                comparisonMapperAttribute) match {
          case Right(
              (label, delimitedPosition, shouldUseDuring, comparisonMapper)) =>
            val metaData = ColumnDescriptionMetaData(delimitedPosition.toString,
                                                     label,
                                                     shouldUseDuring)
            Right(
              DelimitedColumnDescription(metaData,
                                         delimitedPosition,
                                         comparisonMapper))
          case Left(errorMessage) =>
            Left(errorMessage)
        }
      case _ =>
        val bunchOfErrors = BunchOfErrors(unknownAttributeList)
        DelimitedColumnDescriptionErrorMessages.invalidAttributes(bunchOfErrors)
    }
  }

  private def validateAttributeValues(
      labelAttributeValue: Either[UnrecoverableError, String],
      positionComponent: Either[UnrecoverableError, DelimitedPosition],
      shouldUseDuringComponent: Either[UnrecoverableError, ShouldUseDuring],
      valueMapperComponent: Either[UnrecoverableError, ValueMapper]
  ): Either[UnrecoverableError,
            (String, DelimitedPosition, ShouldUseDuring, ValueMapper)] =
    labelAttributeValue match {

      case Right(labelAttributeValue) =>
        (positionComponent, shouldUseDuringComponent, valueMapperComponent) match {
          case (Right(position), Right(shouldUseDuring), Right(valueMapper)) =>
            val classParameters =
              (labelAttributeValue, position, shouldUseDuring, valueMapper)
            Right(classParameters)
          case _ =>
            val errorMessages = UnrecoverableError.collectAllErrorsToList(
              positionComponent,
              shouldUseDuringComponent,
              valueMapperComponent)
            DelimitedColumnDescriptionErrorMessages.invalidAttributes(
              labelAttributeValue,
              errorMessages)
        }

      case Left(labelAttributeErrorMessage) =>
        val otherErrors = UnrecoverableError.collectAllErrorsToList(
          positionComponent,
          shouldUseDuringComponent,
          valueMapperComponent)
        DelimitedColumnDescriptionErrorMessages.invalidAttributes(
          labelAttributeErrorMessage :: otherErrors)
    }

  private val labelKey =
    AttributeValueExtractor("label", AttributeValueConversion.extractValue)
  val listOfAttributesKeys = labelKey :: ShouldUseDuring.listOfAttributesKeys ::: ValueMapper.listOfAttributesKeys ::: DelimitedPosition.listOfAttributesKeys
}
