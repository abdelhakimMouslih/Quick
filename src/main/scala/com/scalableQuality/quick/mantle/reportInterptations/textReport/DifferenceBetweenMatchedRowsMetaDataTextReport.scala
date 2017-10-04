package com.scalableQuality.quick.mantle.reportInterptations.textReport

import com.scalableQuality.quick.core.Reporting.DifferenceBetweenMatchedRowsMetaData

object DifferenceBetweenMatchedRowsMetaDataTextReport {
  def apply(
             metaData: DifferenceBetweenMatchedRowsMetaData
           ): List[String] = {
    val rowLabel = s"Two rows of type ${metaData.rowLabel} are different"
    val leftFileRowNumber = s"row at line number ${metaData.leftFileRowLineNumber} in the file ${metaData.leftFileLabel}"
    val rightFileRowNumber = s"row at line number ${metaData.rightFileRowLineNumber} in the file ${metaData.rightFileLabel}"
    rowLabel :: leftFileRowNumber :: rightFileRowNumber :: Nil
  }
}
