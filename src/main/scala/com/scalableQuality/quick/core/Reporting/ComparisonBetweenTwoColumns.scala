package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.Reporting.InvalidColumns.FailedChecksColumns
import com.scalableQuality.quick.core.fileComponentDescriptions.ColumnDescriptionMetaData
import com.scalableQuality.quick.core.phases.{ReportingStage, ValidationStage}

sealed trait ComparisonBetweenTwoColumns

object ComparisonBetweenTwoColumns {

  def apply(
      columnDescriptionMetaData: ColumnDescriptionMetaData,
      leftFileColumnValue: => Option[String],
      rightFileColumnValue: => Option[String],
      theTwoColumnsAreEquivalent: => Boolean,
      passedChecksSuccessfully: => Boolean
  ): ComparisonBetweenTwoColumns = if (passedChecksSuccessfully) {
    val shouldUseInReporting =
      columnDescriptionMetaData.shouldUseDuring(ReportingStage)
    val shouldUseInValidation =
      columnDescriptionMetaData.shouldUseDuring(ValidationStage)
    if (shouldUseInValidation) {
      if (theTwoColumnsAreEquivalent && shouldUseInReporting) {
        ReportingColumns(
          leftFileColumnValue,
          rightFileColumnValue,
          columnDescriptionMetaData.position,
          columnDescriptionMetaData.label
        )
      } else if (theTwoColumnsAreEquivalent) {
        ValidColumns
      } else {
        InvalidColumns(
          leftFileColumnValue,
          rightFileColumnValue,
          columnDescriptionMetaData.position,
          columnDescriptionMetaData.label
        )
      }
    } else if (shouldUseInReporting) {
      ReportingColumns(
        leftFileColumnValue,
        rightFileColumnValue,
        columnDescriptionMetaData.position,
        columnDescriptionMetaData.label
      )
    } else {
      IrrelevantColumns
    }

  } else {
    FailedChecksColumns(
      leftFileColumnValue,
      rightFileColumnValue,
      columnDescriptionMetaData.position,
      columnDescriptionMetaData.label
    )
  }
}

// represents the columns useDuringValidation = true  && useDuringReporting = false && leftColumn == rightColumn
case object ValidColumns extends ComparisonBetweenTwoColumns

// represents the columns useDuringValidation = false  && useDuringReporting = false
case object IrrelevantColumns extends ComparisonBetweenTwoColumns

// represents the columns
// useDuringReporting = true && (useDuringValidation = false || (useDuringValidation = true && leftColumn == rightColumn))
class ReportingColumns(
    leftColumnValue: => Option[String],
    rightColumnValue: => Option[String],
    val columnPosition: String,
    val columnLabel: String
) extends ComparisonBetweenTwoColumns {
  lazy val leftFileColumnValue: Option[String] = leftColumnValue
  lazy val rightFileColumnValue: Option[String] = rightColumnValue

  override def equals(that: scala.Any): Boolean = that match {
    case reportingColumn: ReportingColumns =>
      reportingColumn.leftFileColumnValue == this.leftFileColumnValue &&
        reportingColumn.rightFileColumnValue == this.rightFileColumnValue &&
        reportingColumn.columnPosition == this.columnPosition &&
        reportingColumn.columnLabel == this.columnLabel
    case _ => false
  }

  override def toString: String =
    s" ${this.leftFileColumnValue} ${this.rightFileColumnValue} $columnPosition"

}

object ReportingColumns {
  def apply(
      leftColumnValue: => Option[String],
      rightColumnValue: => Option[String],
      columnPosition: String,
      columnLabel: String
  ): ReportingColumns =
    new ReportingColumns(leftColumnValue,
                         rightColumnValue,
                         columnPosition,
                         columnLabel)

  /*def apply(
             columnDescription: FixedColumnDescription,
             leftFileRawRow : Option[RawRow],
             rightFileRawRow : Option[RawRow]
           ): ReportingColumns = ReportingColumns(
    leftFileRawRow.flatMap(columnDescription.columnValue),
    rightFileRawRow.flatMap(columnDescription.columnValue),
    columnDescription.metaData.position,
    columnDescription.metaData.label
  )*/
}

// represents the columns
// useDuringValidation = true && leftColumn != rightColumn
class InvalidColumns(
    leftColumnValue: => Option[String],
    rightColumnValue: => Option[String],
    val columnPosition: String,
    val columnLabel: String
) extends ComparisonBetweenTwoColumns {
  lazy val leftFileColumnValue = leftColumnValue
  lazy val rightFileColumnValue = rightColumnValue
  override def equals(that: scala.Any): Boolean = that match {
    case invalidColumn: InvalidColumns =>
      invalidColumn.leftFileColumnValue == this.leftFileColumnValue &&
      invalidColumn.rightFileColumnValue == this.rightFileColumnValue &&
      invalidColumn.columnPosition == this.columnPosition //&&
      invalidColumn.columnLabel == this.columnLabel
    case _ => false
  }

  override def toString: String =
    s" ${this.leftFileColumnValue} ${this.rightFileColumnValue} $columnPosition"

}

object InvalidColumns {
  def apply(
      leftColumnValue: => Option[String],
      rightColumnValue: => Option[String],
      columnPosition: String,
      columnLabel: String
  ): InvalidColumns =
    new InvalidColumns(leftColumnValue,
                       rightColumnValue,
                       columnPosition,
                       columnLabel)




  class FailedChecksColumns(
                        leftColumnValue: => Option[String],
                        rightColumnValue: => Option[String],
                        val columnPosition: String,
                        val columnLabel: String
                      ) extends ComparisonBetweenTwoColumns {
    lazy val leftFileColumnValue = leftColumnValue
    lazy val rightFileColumnValue = rightColumnValue
    override def equals(that: scala.Any): Boolean = that match {
      case failedChecksColumns: FailedChecksColumns =>
        failedChecksColumns.leftFileColumnValue == this.leftFileColumnValue &&
          failedChecksColumns.rightFileColumnValue == this.rightFileColumnValue &&
          failedChecksColumns.columnPosition == this.columnPosition //&&
        failedChecksColumns.columnLabel == this.columnLabel
      case _ => false
    }

    override def toString: String =
      s" ${this.leftFileColumnValue} ${this.rightFileColumnValue} $columnPosition"

  }

  object FailedChecksColumns {
    def apply(
               leftColumnValue: => Option[String],
               rightColumnValue: => Option[String],
               columnPosition: String,
               columnLabel: String
             ): FailedChecksColumns =
      new FailedChecksColumns(leftColumnValue,
        rightColumnValue,
        columnPosition,
        columnLabel)
  }
}
