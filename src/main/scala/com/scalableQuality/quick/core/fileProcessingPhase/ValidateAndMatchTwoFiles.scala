package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.Reporting.{
  FilledValidationAndMatchingReport,
  ValidationAndMatchingReport
}

class ValidateAndMatchTwoFiles(
    val validationAndMatchingProcesses: List[RowsProcessingPhase]
) {
  lazy val validationAndMatchingReports
    : List[FilledValidationAndMatchingReport] = {
    val reports = executeValidationAndMatchingProcesses(
      this.validationAndMatchingProcesses)
    ValidationAndMatchingReport.keepOnlyFilledReports(reports)
  }

  private def executeValidationAndMatchingProcesses(
      processes: List[RowsProcessingPhase]
  ): List[ValidationAndMatchingReport] =
    processes.map(_.execute)

}

object ValidateAndMatchTwoFiles {
  def apply(
      validationAndMatchingProcesses: List[RowsProcessingPhase]
  ): ValidateAndMatchTwoFiles =
    new ValidateAndMatchTwoFiles(validationAndMatchingProcesses)
}
