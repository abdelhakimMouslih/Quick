package com.scalableQuality.quick.core.Reporting

import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

class ValidationAndMatchingReport(
                                           listOfDifferencesBetweenMatchedRows: Stream[DifferenceBetweenMatchedRows]
                                         ) {
  def interpret[Interpretation](
                                 interpreter: DifferenceBetweenMatchedRows => Interpretation
                               ): Stream[Interpretation] = listOfDifferencesBetweenMatchedRows.map(interpreter)

}


object ValidationAndMatchingReport {
  def apply(
             orderedRowDescription: OrderedRowDescription,
             leftFileLabel: Option[String],
             rightFileLabel: Option[String],
             matchedRows: List[(Option[RawRow], Option[RawRow])]
           ): ValidationAndMatchingReport = {
    val listOfDifferencesBetweenMatchedRows = matchedRows.toStream.map{
      twoMatchedRows => DifferenceBetweenMatchedRows(
        orderedRowDescription,
        leftFileLabel,
        rightFileLabel,
        twoMatchedRows
      )
    }
    ValidationAndMatchingReport(listOfDifferencesBetweenMatchedRows)
  }

  def apply(
             listOfDifferencesBetweenMatchedRows: Stream[DifferenceBetweenMatchedRows]
           ): ValidationAndMatchingReport =
    new ValidationAndMatchingReport(listOfDifferencesBetweenMatchedRows)
}