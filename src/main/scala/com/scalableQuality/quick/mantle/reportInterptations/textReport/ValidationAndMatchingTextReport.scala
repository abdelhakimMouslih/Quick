package com.scalableQuality.quick.mantle.reportInterptations.textReport

import com.scalableQuality.quick.core.Reporting.ValidationAndMatchingReport

object ValidationAndMatchingTextReport {
  def apply(
             validationAndMatchingTextReport: ValidationAndMatchingReport
           ): Stream[List[String]] = {
    validationAndMatchingTextReport.interpret(DifferenceBetweenMatchedRowsTextReport(_))
  }
}
