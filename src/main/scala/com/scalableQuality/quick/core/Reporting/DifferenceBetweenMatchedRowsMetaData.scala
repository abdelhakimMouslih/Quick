package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

case class DifferenceBetweenMatchedRowsMetaData(
    leftFileLabel: String,
    rightFileLabel: String,
    rowLabel: String,
    leftFileRowLineNumber: String,
    rightFileRowLineNumber: String
)

object DifferenceBetweenMatchedRowsMetaData {
  def apply(
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      orderedRowDescription: OrderedRowDescription,
      leftFileRawRow: Option[RawRow],
      rightFileRawRow: Option[RawRow]
  ): DifferenceBetweenMatchedRowsMetaData =
    DifferenceBetweenMatchedRowsMetaData(
      leftFileLabel.getOrElse("Left file"),
      rightFileLabel.getOrElse("Right file"),
      orderedRowDescription.label,
      leftFileRawRow.map(_.lineNumber.toString).getOrElse("N/A"),
      rightFileRawRow.map(_.lineNumber.toString).getOrElse("N/A")
    )
}
