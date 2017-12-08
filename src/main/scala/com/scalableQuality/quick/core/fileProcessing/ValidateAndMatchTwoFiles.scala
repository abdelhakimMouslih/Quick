package com.scalableQuality.quick.core.fileProcessing

import com.scalableQuality.quick.core.Reporting.{EmptyValidationAndMatchingReport, FilledValidationAndMatchingReport, ValidationAndMatchingReport}


class ValidateAndMatchTwoFiles(
                                val validationAndMatchingProcesses : List[() => ValidationAndMatchingReport]
                              ) {
  lazy val validationAndMatchingReports : List[FilledValidationAndMatchingReport] = {
    val reports = executeValidationAndMatchingProcesses(this.validationAndMatchingProcesses)
    keepOnlyFilledReports(reports)
  }

  private def executeValidationAndMatchingProcesses(
                                                     processes : List[() => ValidationAndMatchingReport]
                                                   ): List[ValidationAndMatchingReport] =
    processes.map(_())

  private def keepOnlyFilledReports(reports: List[ValidationAndMatchingReport]): List[FilledValidationAndMatchingReport] = {
    def loop(
              allReports: List[ValidationAndMatchingReport],
              filledReports : List[FilledValidationAndMatchingReport]
            ): List[FilledValidationAndMatchingReport] = allReports match {
      case Nil => filledReports.reverse
      case EmptyValidationAndMatchingReport :: restOfReports => loop(restOfReports, filledReports)
      case (filledReport: FilledValidationAndMatchingReport) :: restOfReports =>
        loop(restOfReports, filledReport:: filledReports)
    }
    loop(reports, Nil)
  }
}

object ValidateAndMatchTwoFiles {
  def apply(
             validationAndMatchingProcesses: List[() => ValidationAndMatchingReport]
           ): ValidateAndMatchTwoFiles = new ValidateAndMatchTwoFiles(validationAndMatchingProcesses)
}