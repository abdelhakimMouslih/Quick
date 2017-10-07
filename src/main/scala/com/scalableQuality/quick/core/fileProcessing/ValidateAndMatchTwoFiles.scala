package com.scalableQuality.quick.core.fileProcessing

import com.scalableQuality.quick.core.Reporting.ValidationAndMatchingReport


class ValidateAndMatchTwoFiles(
                                val validationAndMatchingProcesses : List[() => ValidationAndMatchingReport]
                              ) {
  lazy val validationAndMatchingReports : List[ValidationAndMatchingReport] = validationAndMatchingProcesses.map(_())
}

object ValidateAndMatchTwoFiles {
  def apply(
             validationAndMatchingProcesses: List[() => ValidationAndMatchingReport]
           ): ValidateAndMatchTwoFiles = new ValidateAndMatchTwoFiles(validationAndMatchingProcesses)
}