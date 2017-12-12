package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.Reporting.ValidationAndMatchingReport
import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

class MatchRows(
    orderedRowDescription: OrderedRowDescription,
    leftFileRows: List[RawRow],
    rightFileRows: List[RawRow],
    leftFileLabel: Option[String],
    rightFileLabel: Option[String]
) extends RowsProcessingPhase {
  override def execute(): ValidationAndMatchingReport = {
    val matchedRows =
      MatchingProcess(orderedRowDescription, leftFileRows, rightFileRows)
    ValidationAndMatchingReport(
      orderedRowDescription,
      leftFileLabel,
      rightFileLabel,
      matchedRows
    )
  }
}

object MatchRows {
  def apply(
      orderedRowDescription: OrderedRowDescription,
      leftFileRows: List[RawRow],
      rightFileRows: List[RawRow],
      leftFileLabel: Option[String],
      rightFileLabel: Option[String]
  ): MatchRows = new MatchRows(
    orderedRowDescription,
    leftFileRows,
    rightFileRows,
    leftFileLabel,
    rightFileLabel
  )
}
