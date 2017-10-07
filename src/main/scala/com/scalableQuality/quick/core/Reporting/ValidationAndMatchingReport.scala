package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

class ValidationAndMatchingReport(
                                           val listOfDifferencesBetweenMatchedRows: List[ () => DifferenceBetweenMatchedRows]
                                         ) {
  def interpret[Interpretation](
                                 interpreter: DifferenceBetweenMatchedRows => Interpretation
                               ): List[() => Interpretation] = {
    val thisLazyInterpretation = lazyInterpretation(interpreter)(_)
    listOfDifferencesBetweenMatchedRows.map(thisLazyInterpretation)
  }

  def lazyInterpretation[Interpretation]
  (
    interpreter: DifferenceBetweenMatchedRows => Interpretation
  )(
    differenceBetweenMatchedRows: () => DifferenceBetweenMatchedRows
  )
  : () => Interpretation = () => {
    val difference = differenceBetweenMatchedRows()
    interpreter(difference)
  }

}


object ValidationAndMatchingReport {
  def apply(
             orderedRowDescription: OrderedRowDescription,
             leftFileLabel: Option[String],
             rightFileLabel: Option[String],
             matchedRows: List[(Option[RawRow], Option[RawRow])]
           ): ValidationAndMatchingReport = {

    val listOfDifferencesBetweenMatchedRows = matchedRows.map{
      twoMatchedRows => () => {
        DifferenceBetweenMatchedRows(
          orderedRowDescription,
          leftFileLabel,
          rightFileLabel,
          twoMatchedRows
        )
      }
    }
    ValidationAndMatchingReport(listOfDifferencesBetweenMatchedRows)
  }

  private def apply(
                     listOfDifferencesBetweenMatchedRows: List[ () => DifferenceBetweenMatchedRows]
                   ): ValidationAndMatchingReport =
    new ValidationAndMatchingReport(listOfDifferencesBetweenMatchedRows)
}