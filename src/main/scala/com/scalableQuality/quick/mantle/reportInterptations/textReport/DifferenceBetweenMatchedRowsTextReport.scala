package com.scalableQuality.quick.mantle.reportInterptations.textReport

import com.scalableQuality.quick.core.Reporting.DifferenceBetweenMatchedRows

object DifferenceBetweenMatchedRowsTextReport {
  def apply(
             differenceBetweenMatchedRows: DifferenceBetweenMatchedRows
           ):List[String] = {
    val metaData = DifferenceBetweenMatchedRowsMetaDataTextReport(differenceBetweenMatchedRows.metaData)
    val tableOfColumnComparison = ColumnComparisonTable(differenceBetweenMatchedRows)
    metaData ::: tableOfColumnComparison
  }
}
