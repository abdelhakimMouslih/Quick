package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.Reporting.{
  FilledValidationAndMatchingReport,
  ValidationAndMatchingReport
}
import com.scalableQuality.quick.surface.commandLineOptions.QuickState

class ValidateAndMatchTwoFiles(
    val validationAndMatchingProcesses: List[RowsProcessingPhase],
    quickState: QuickState
) {
  lazy val validationAndMatchingReports
    : List[FilledValidationAndMatchingReport] = {
    val reports = quickState.rowsProcessingPhaseExecution(
      this.validationAndMatchingProcesses)
    ValidationAndMatchingReport.keepOnlyFilledReports(reports)
  }

}

object ValidateAndMatchTwoFiles {
  def apply(
      validationAndMatchingProcesses: List[RowsProcessingPhase],
      quickState: QuickState
  ): ValidateAndMatchTwoFiles =
    new ValidateAndMatchTwoFiles(validationAndMatchingProcesses, quickState)
}
