package com.scalableQuality.quick.mantle.reportInterptations.textReport

import com.scalableQuality.quick.core.Reporting.ValidationAndMatchingReport

object ValidationAndMatchingTextReport {
  def apply(
             validationAndMatchingTextReport: ValidationAndMatchingReport
           ): List[ () => List[String] ] = {
    validationAndMatchingTextReport.interpret(DifferenceBetweenMatchedRowsTextReport(_))
  }
}
