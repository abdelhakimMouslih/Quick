package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.fileComponentDescripts.{ColumnDescription, ColumnPosition}
import com.scalableQuality.quick.core.others.{MatchingStage, ValidationStage}

sealed trait ComparisonBetweenTwoColumns

object ComparisonBetweenTwoColumns {
  def apply(
           columnDescription: ColumnDescription,
           leftFileColumnValue : => Option[String],
           rightFileColumnValue: => Option[String],
           theTwoColumnsAreEquivalent: => Boolean
           ): ComparisonBetweenTwoColumns = {
    val shouldUseInMatching = columnDescription.shouldUseDuring(MatchingStage)
    val shouldUseInValidation = columnDescription.shouldUseDuring(ValidationStage)

    if (shouldUseInValidation) {
      if (theTwoColumnsAreEquivalent && shouldUseInMatching ) {
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
    } else if (shouldUseInMatching) {
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

case object ValidColumns extends  ComparisonBetweenTwoColumns
case object IrrelevantColumns extends ComparisonBetweenTwoColumns


class ReportingColumns(
                        leftColumnValue: => Option[String],
                        rightColumnValue: => Option[String],
                        val  columnPosition: ColumnPosition,
                        val columnLabel: String
                      ) extends ComparisonBetweenTwoColumns {
  lazy val leftFileColumnValue: Option[String] = leftColumnValue
  lazy val rightFileColumnValue: Option[String] = rightColumnValue
}

object ReportingColumns {
  def apply(
             leftColumnValue: => Option[String],
             rightColumnValue: => Option[String],
             columnPosition: ColumnPosition,
             columnLabel: String
           ): ReportingColumns = new ReportingColumns(leftColumnValue,rightColumnValue,columnPosition,columnLabel)
}






class InvalidColumns(
                      leftColumnValue: => Option[String],
                      rightColumnValue: => Option[String],
                      val columnPosition: ColumnPosition,
                      val columnLabel: String
                    ) extends ComparisonBetweenTwoColumns {
  lazy val leftFileColumnValue = leftColumnValue
  lazy val rightFileColumnValue = rightColumnValue
}

object InvalidColumns {
  def apply(
             leftColumnValue: => Option[String],
             rightColumnValue: => Option[String],
             columnPosition: ColumnPosition,
             columnLabel: String
           ): InvalidColumns = new InvalidColumns(leftColumnValue, rightColumnValue, columnPosition, columnLabel )
}


