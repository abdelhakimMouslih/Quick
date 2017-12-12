package com.scalableQuality.quick.surface.output

object WriteTextReportToStdout {
  def apply(
      textReports: List[() => List[String]]
  ): Unit = BufferedWriteToStdout(textReports)
}
