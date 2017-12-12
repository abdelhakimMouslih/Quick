package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

case class DifferenceBetweenMatchedRows(
    val metaData: DifferenceBetweenMatchedRowsMetaData,
    val columnComparisons: List[ComparisonBetweenTwoColumns]
)

object DifferenceBetweenMatchedRows {
  def apply(
      orderedRowDescription: OrderedRowDescription,
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      matchedRows: (Option[RawRow], Option[RawRow])
  ): DifferenceBetweenMatchedRows = {
    val leftRawRow = matchedRows._1
    val rightRawRow = matchedRows._2
    val metaData = DifferenceBetweenMatchedRowsMetaData(
      leftFileLabel,
      rightFileLabel,
      orderedRowDescription,
      leftRawRow,
      rightRawRow
    )
    val columnComparisons =
      orderedRowDescription.compare(leftRawRow, rightRawRow)
    DifferenceBetweenMatchedRows(metaData, columnComparisons)
  }

}
