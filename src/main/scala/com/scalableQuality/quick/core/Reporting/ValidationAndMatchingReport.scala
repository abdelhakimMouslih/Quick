package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.fileComponentDescriptions.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

sealed trait ValidationAndMatchingReport

object ValidationAndMatchingReport {
  def apply(
      orderedRowDescription: OrderedRowDescription,
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      matchedRows: List[(Option[RawRow], Option[RawRow])]
  ): ValidationAndMatchingReport = matchedRows match {
    case Nil => EmptyValidationAndMatchingReport
    case _ =>
      FilledValidationAndMatchingReport(
        orderedRowDescription,
        leftFileLabel,
        rightFileLabel,
        matchedRows
      )
  }
  def keepOnlyFilledReports(reports: List[ValidationAndMatchingReport])
    : List[FilledValidationAndMatchingReport] = {
    def loop(
        allReports: List[ValidationAndMatchingReport],
        filledReports: List[FilledValidationAndMatchingReport]
    ): List[FilledValidationAndMatchingReport] = allReports match {
      case Nil => filledReports.reverse
      case EmptyValidationAndMatchingReport :: restOfReports =>
        loop(restOfReports, filledReports)
      case (filledReport: FilledValidationAndMatchingReport) :: restOfReports =>
        loop(restOfReports, filledReport :: filledReports)
    }
    loop(reports, Nil)
  }
}

case object EmptyValidationAndMatchingReport extends ValidationAndMatchingReport

class FilledValidationAndMatchingReport(
    val listOfDifferencesBetweenMatchedRows: List[
      () => DifferenceBetweenMatchedRows]
) extends ValidationAndMatchingReport {
  def interpret[Interpretation](
      interpreter: DifferenceBetweenMatchedRows => Interpretation
  ): List[() => Interpretation] = {
    val thisLazyInterpretation = lazyInterpretation(interpreter)(_)
    listOfDifferencesBetweenMatchedRows.map(thisLazyInterpretation)
  }

  def lazyInterpretation[Interpretation](
      interpreter: DifferenceBetweenMatchedRows => Interpretation
  )(
      differenceBetweenMatchedRows: () => DifferenceBetweenMatchedRows
  ): () => Interpretation = () => {
    val difference = differenceBetweenMatchedRows()
    interpreter(difference)
  }

}

object FilledValidationAndMatchingReport {
  def apply(
      orderedRowDescription: OrderedRowDescription,
      leftFileLabel: Option[String],
      rightFileLabel: Option[String],
      matchedRows: List[(Option[RawRow], Option[RawRow])]
  ): FilledValidationAndMatchingReport = {

    val listOfDifferencesBetweenMatchedRows = matchedRows.map {
      twoMatchedRows => () =>
        {
          DifferenceBetweenMatchedRows(
            orderedRowDescription,
            leftFileLabel,
            rightFileLabel,
            twoMatchedRows
          )
        }
    }
    FilledValidationAndMatchingReport(listOfDifferencesBetweenMatchedRows)
  }

  private def apply(
      listOfDifferencesBetweenMatchedRows: List[
        () => DifferenceBetweenMatchedRows]
  ): FilledValidationAndMatchingReport =
    new FilledValidationAndMatchingReport(listOfDifferencesBetweenMatchedRows)
}
