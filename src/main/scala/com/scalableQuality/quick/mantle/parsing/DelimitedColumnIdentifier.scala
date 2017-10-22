package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts.DelimitedColumnDescription
import com.scalableQuality.quick.core.others.MatchAgainst
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.MetaData

class DelimitedColumnIdentifier(
                                 matchAgainst: MatchAgainst,
                                 columnDescription: DelimitedColumnDescription
                               ) {
  def apply(splitRow: Vector[String]): Boolean = {
    val extractedColumn = columnDescription.columnValue(splitRow)
    extractedColumn.map(matchAgainst(_)).getOrElse(RowToRowDescriptionMatcher.defaultIdentificationResult)
  }
}

object DelimitedColumnIdentifier {
  def apply(
             matchAgainst: MatchAgainst,
             columnDescription: DelimitedColumnDescription
           ): DelimitedColumnIdentifier = new DelimitedColumnIdentifier(matchAgainst, columnDescription)

  def apply(elemMetaData: MetaData): Either[ErrorMessage,(DelimitedColumnDescription, DelimitedColumnIdentifier)] = {

    val columnDescriptionEither = DelimitedColumnDescription(elemMetaData)

    val matchAgainstEither = MatchAgainst(elemMetaData)

    (columnDescriptionEither, matchAgainstEither) match {
      case (Left(errorMessage), _) =>
        makeErrorMessage(errorMessage)
      case (_, Left(errorMessage)) =>
        makeErrorMessage(errorMessage)
      case (Right(columnDescription), Right(matchAgainst)) =>
        val columnIdentifier = DelimitedColumnIdentifier(matchAgainst, columnDescription)
        val result = (columnDescription, columnIdentifier)
        Right(result)
    }
  }
  private def makeErrorMessage(childErrorMessage: ErrorMessage) :
  Either[ErrorMessage,(DelimitedColumnDescription, DelimitedColumnIdentifier)] = {
    val errorMessage = UnrecoverableError(
      "creating a column identifier",
      "encountered a problem below",
      "solve the problems below",
      List(childErrorMessage)
    )
    Left(errorMessage)
  }
}
