package com.scalableQuality.quick.core.fileProcessing

import com.scalableQuality.quick.core.Reporting.{FilledValidationAndMatchingReport, ValidationAndMatchingReport}
import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

object ValidateAndMatchRows {
  def apply(
           orderedRowDescription: OrderedRowDescription,
           leftFileRows: List[RawRow],
           rightFileRows: List[RawRow],
           leftFileLabel: Option[String],
           rightFileLabel: Option[String]
           ): ValidationAndMatchingReport = {
    val invalidRowsFromTwoFiles = ValidationProcess(orderedRowDescription, leftFileRows, rightFileRows )
    val matchedRows = MatchingProcess(orderedRowDescription, invalidRowsFromTwoFiles._1, invalidRowsFromTwoFiles._2)
    ValidationAndMatchingReport(
      orderedRowDescription,
      leftFileLabel,
      rightFileLabel,
      matchedRows
    )
  }
}
