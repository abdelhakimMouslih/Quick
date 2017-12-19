package com.scalableQuality.quick.mantle.reportInterptations.textReport

import com.scalableQuality.quick.core.Reporting.DifferenceBetweenMatchedRowsMetaData

object DifferenceBetweenMatchedRowsMetaDataTextReport {
  def apply(
      metaData: DifferenceBetweenMatchedRowsMetaData
  ): List[String] = {
    val rowLabel =
      s"comparison of two rows using the row description: ${metaData.rowLabel}"
    val leftFileRowNumber =
      s"from file ${metaData.leftFileLabel} the row at line ${metaData.leftFileRowLineNumber} is used"
    val rightFileRowNumber =
      s"from file ${metaData.rightFileLabel} the row at line ${metaData.rightFileRowLineNumber} is used"
    rowLabel :: leftFileRowNumber :: rightFileRowNumber :: Nil
  }
}
