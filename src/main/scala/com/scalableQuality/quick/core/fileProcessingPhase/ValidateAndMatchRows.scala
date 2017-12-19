package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.Reporting.{
  FilledValidationAndMatchingReport,
  ValidationAndMatchingReport
}
import com.scalableQuality.quick.core.fileComponentDescriptions.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow
class ValidateAndMatchRows(
    orderedRowDescription: OrderedRowDescription,
    leftFileRows: List[RawRow],
    rightFileRows: List[RawRow],
    leftFileLabel: Option[String],
    rightFileLabel: Option[String]
) extends RowsProcessingPhase {
  override def execute(): ValidationAndMatchingReport = {
    val invalidRowsFromTwoFiles =
      ValidationProcess(orderedRowDescription, leftFileRows, rightFileRows)
    val matchedRows = MatchingProcess(orderedRowDescription,
                                      invalidRowsFromTwoFiles._1,
                                      invalidRowsFromTwoFiles._2)
    ValidationAndMatchingReport(
      orderedRowDescription,
      leftFileLabel,
      rightFileLabel,
      matchedRows
    )
  }
}
object ValidateAndMatchRows {
  def apply(
      orderedRowDescription: OrderedRowDescription,
      leftFileRows: List[RawRow],
      rightFileRows: List[RawRow],
      leftFileLabel: Option[String],
      rightFileLabel: Option[String]
  ): ValidateAndMatchRows = new ValidateAndMatchRows(
    orderedRowDescription,
    leftFileRows,
    rightFileRows,
    leftFileLabel,
    rightFileLabel
  )
}
