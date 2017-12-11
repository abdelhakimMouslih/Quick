package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.Reporting.ValidationAndMatchingReport

trait RowsProcessingPhase {
  def execute(): ValidationAndMatchingReport
}
