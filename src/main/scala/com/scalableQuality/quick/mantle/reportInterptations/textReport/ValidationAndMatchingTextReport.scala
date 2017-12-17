package com.scalableQuality.quick.mantle.reportInterptations.textReport

import com.scalableQuality.quick.core.Reporting.FilledValidationAndMatchingReport

object ValidationAndMatchingTextReport {
  def apply(
      validationAndMatchingTextReport: FilledValidationAndMatchingReport
  ): List[() => List[String]] = {
    validationAndMatchingTextReport.interpret(
      DifferenceBetweenMatchedRowsTextReport(_))
  }
}
