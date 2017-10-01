package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.fileComponentDescripts.{ColumnDescription, ColumnPosition}
import com.scalableQuality.quick.core.others.{MatchingStage, ReportingStage, ValidationStage}
import com.scalableQuality.quick.mantle.parsing.RawRow

sealed trait ComparisonBetweenTwoColumns

object ComparisonBetweenTwoColumns {
  def apply(
           columnDescription: ColumnDescription,
           leftFileRow: Option[RawRow],
           rightFileRow: Option[RawRow]
           ): ComparisonBetweenTwoColumns = ComparisonBetweenTwoColumns(
    columnDescription,
    leftFileRow.flatMap(columnDescription.columnValue),
    rightFileRow.flatMap(columnDescription.columnValue),
    leftFileRow.flatMap(columnDescription.comparisonValue) == rightFileRow.flatMap(columnDescription.comparisonValue)
  )
  def apply(
           columnDescription: ColumnDescription,
           leftFileColumnValue : => Option[String],
           rightFileColumnValue: => Option[String],
           theTwoColumnsAreEquivalent: => Boolean
           ): ComparisonBetweenTwoColumns = {
    val shouldUseInReporting = columnDescription.shouldUseDuring(ReportingStage)
    val shouldUseInValidation = columnDescription.shouldUseDuring(ValidationStage)

    if (shouldUseInValidation) {
      if (theTwoColumnsAreEquivalent &&  shouldUseInReporting ) {
        ReportingColumns(
          leftFileColumnValue,
          rightFileColumnValue,
          columnDescription.position,
          columnDescription.label
        )
      } else if (theTwoColumnsAreEquivalent) {
        ValidColumns
      } else {
        InvalidColumns(
          leftFileColumnValue,
          rightFileColumnValue,
          columnDescription.position,
          columnDescription.label
        )
      }
    } else if (shouldUseInReporting) {
      ReportingColumns(
        leftFileColumnValue,
        rightFileColumnValue,
        columnDescription.position,
        columnDescription.label
      )
    } else {
      IrrelevantColumns
    }

  }
}

// represents the columns useDuringValidation = true  && useDuringReporting = false && leftColumn == rightColumn
case object ValidColumns extends  ComparisonBetweenTwoColumns

// represents the columns useDuringValidation = false  && useDuringReporting = false
case object IrrelevantColumns extends ComparisonBetweenTwoColumns

// represents the columns
// useDuringReporting = true && (useDuringValidation = false || (useDuringValidation = true && leftColumn == rightColumn))
class ReportingColumns(
                        leftColumnValue: => Option[String],
                        rightColumnValue: => Option[String],
                        val  columnPosition: ColumnPosition,
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
}

object ReportingColumns {
  def apply(
             leftColumnValue: => Option[String],
             rightColumnValue: => Option[String],
             columnPosition: ColumnPosition,
             columnLabel: String
           ): ReportingColumns = new ReportingColumns(leftColumnValue,rightColumnValue,columnPosition,columnLabel)
  def apply(
           columnDescription: ColumnDescription,
           leftFileRawRow : Option[RawRow],
           rightFileRawRow : Option[RawRow]
           ): ReportingColumns = ReportingColumns(
    leftFileRawRow.flatMap(columnDescription.columnValue),
    rightFileRawRow.flatMap(columnDescription.columnValue),
    columnDescription.position,
    columnDescription.label
  )
}

// represents the columns
// useDuringValidation = true && leftColumn != rightColumn
class InvalidColumns(
                      leftColumnValue: => Option[String],
                      rightColumnValue: => Option[String],
                      val columnPosition: ColumnPosition,
                      val columnLabel: String
                    ) extends ComparisonBetweenTwoColumns {
  lazy val leftFileColumnValue = leftColumnValue
  lazy val rightFileColumnValue = rightColumnValue
  override def equals(that: scala.Any): Boolean = that match {
    case invalidColumn: InvalidColumns =>
      invalidColumn.leftFileColumnValue == this.leftFileColumnValue &&
        invalidColumn.rightFileColumnValue == this.rightFileColumnValue &&
        invalidColumn.columnPosition == this.columnPosition &&
        invalidColumn.columnLabel == this.columnLabel
    case _ => false
  }
}

object InvalidColumns {
  def apply(
             leftColumnValue: => Option[String],
             rightColumnValue: => Option[String],
             columnPosition: ColumnPosition,
             columnLabel: String
           ): InvalidColumns = new InvalidColumns(leftColumnValue, rightColumnValue, columnPosition, columnLabel )

  def apply(
             columnDescription: ColumnDescription,
             leftFileRawRow : Option[RawRow],
             rightFileRawRow : Option[RawRow]
           ): InvalidColumns = InvalidColumns(
    leftFileRawRow.flatMap(columnDescription.columnValue),
    rightFileRawRow.flatMap(columnDescription.columnValue),
    columnDescription.position,
    columnDescription.label
  )
}


