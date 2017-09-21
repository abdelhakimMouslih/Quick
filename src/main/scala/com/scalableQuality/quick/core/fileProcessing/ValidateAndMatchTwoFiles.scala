package com.scalableQuality.quick.core.fileProcessing

import com.scalableQuality.quick.core.Reporting.ValidationAndMatchingReport


class ValidateAndMatchTwoFiles(
                                validationAndMatchingProcesses : Stream[ValidationAndMatchingReport]
                              ) {

}



object ValidateAndMatchTwoFiles {
  def apply(
             validationAndMatchingProcesses: Stream[ValidationAndMatchingReport]
           ): ValidateAndMatchTwoFiles = new ValidateAndMatchTwoFiles(validationAndMatchingProcesses)
}