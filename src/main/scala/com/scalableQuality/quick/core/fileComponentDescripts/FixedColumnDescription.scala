package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns
import com.scalableQuality.quick.core.fileComponentDescripts.DelimitedColumnDescription.listOfAttributesKeys
import com.scalableQuality.quick.core.fileComponentDescripts.errorMessages.FixedColumnDescriptionErrorMessages
import com.scalableQuality.quick.core.others.{ColumnUsageStages, ShouldUseDuring, ValueMapper}
import com.scalableQuality.quick.mantle.constructFromXml.{AttributeValueConversion, AttributeValueExtractor, AttributesValuesExtractor, XMLHelperFunctions}
import com.scalableQuality.quick.mantle.error.{BunchOfErrors, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.xml.MetaData


case class FixedColumnDescription(
                                   val metaData: ColumnDescriptionMetaData,
                                   val position: FixedPosition,
                                   comparisonMapper: ValueMapper
                               ) {
  def shouldUseDuring(stages: ColumnUsageStages*): Boolean = metaData.shouldUseDuring(stages:_*)
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
  def apply(
             position: FixedPosition,
             shouldUseDuring: ShouldUseDuring,
             comparisonMapper: ValueMapper,
             label: String
           ): FixedColumnDescription = {
    val metaData = ColumnDescriptionMetaData(position.toString, label, shouldUseDuring)
    FixedColumnDescription(metaData, position, comparisonMapper)
  }

  def apply(metaData: MetaData): Either[UnrecoverableError, FixedColumnDescription] = {
    val unknownAttributeList = XMLHelperFunctions.collectUnknownAttributes(listOfAttributesKeys, metaData)
    unknownAttributeList match {
      case Nil =>
        val attributesValues = AttributesValuesExtractor(metaData,labelKey)
        val labelAttributeValue = attributesValues.get(labelKey)

        val fixedPositionEither = FixedPosition(metaData)
        val shouldUseDuringAttribute = ShouldUseDuring(metaData)
        val comparisonMapperAttribute = ValueMapper(metaData)

        validateAttributeValues(labelAttributeValue, fixedPositionEither, shouldUseDuringAttribute, comparisonMapperAttribute) match {
          case Right((label, fixedPosition, shouldUseDuring, comparisonMapper )) =>
            val fixedColumnDescription = FixedColumnDescription(
              fixedPosition,
              shouldUseDuring,
              comparisonMapper,
              label
            )
            Right(fixedColumnDescription)
          case Left(errorMessage) =>
            Left(errorMessage)
        }
      case _ =>
        val bunchOfErrors = BunchOfErrors(unknownAttributeList)
        FixedColumnDescriptionErrorMessages.invalidAttributes(bunchOfErrors)
    }

  }
  private def validateAttributeValues(
                                       labelAttributeValue: Either[UnrecoverableError, String],
                                       positionComponent: Either[UnrecoverableError, FixedPosition],
                                       shouldUseDuringComponent: Either[UnrecoverableError, ShouldUseDuring],
                                       valueMapperComponent: Either[UnrecoverableError, ValueMapper]
                                     ) : Either[UnrecoverableError, (String, FixedPosition, ShouldUseDuring, ValueMapper)] =
    labelAttributeValue match {

      case Right(labelAttributeValue) =>
        (positionComponent, shouldUseDuringComponent, valueMapperComponent) match {
          case (Right(position),Right(shouldUseDuring),Right(valueMapper)) =>
            val classParameters = (labelAttributeValue, position, shouldUseDuring, valueMapper)
            Right(classParameters)
          case _ =>
            val errorMessages = UnrecoverableError.collectAllErrorsToList(positionComponent, shouldUseDuringComponent, valueMapperComponent)
            FixedColumnDescriptionErrorMessages.invalidAttributes(labelAttributeValue, errorMessages)
        }

      case Left(labelAttributeErrorMessage) =>
        val otherErrors = UnrecoverableError.collectAllErrorsToList(positionComponent, shouldUseDuringComponent, valueMapperComponent)
        FixedColumnDescriptionErrorMessages.invalidAttributes(labelAttributeErrorMessage :: otherErrors)
    }

  private val labelKey = AttributeValueExtractor("label", AttributeValueConversion.extractValue)
  val listOfAttributesKeys = labelKey :: ShouldUseDuring.listOfAttributesKeys ::: ValueMapper.listOfAttributesKeys ::: FixedPosition.listOfAttributesKeys

}
