package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts.FixedColumnDescription
import com.scalableQuality.quick.core.others.{MatchAgainst, ValueMapper}
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.MetaData

class FixedColumnIdentifier(
                        matchAgainst: MatchAgainst,
                        columnDescription: FixedColumnDescription
                      ) {
  def apply(row: RawRow): Boolean = {
    val extractedColumn = columnDescription.columnValue(row)
    extractedColumn.map(matchAgainst(_)).getOrElse(RowToRowDescriptionMatcher.defaultIdentificationResult)
  }
}

object FixedColumnIdentifier {

  def apply(
             matchAgainst: MatchAgainst,
             columnDescription: FixedColumnDescription
           ): FixedColumnIdentifier = new FixedColumnIdentifier(matchAgainst, columnDescription)


  def apply(elemMetaData: MetaData): Either[ErrorMessage,(FixedColumnDescription, FixedColumnIdentifier)] = {
    val columnDescriptionEither = FixedColumnDescription(elemMetaData)
    val matchAgainstEither = MatchAgainst(elemMetaData)
    (columnDescriptionEither, matchAgainstEither) match {
      case (Left(errorMessage), _) =>
        makeErrorMessage(errorMessage)
      case (_, Left(errorMessage)) =>
        makeErrorMessage(errorMessage)
      case (Right(columnDescription), Right(matchAgainst)) =>
        val columnIdentifier = FixedColumnIdentifier(matchAgainst, columnDescription)
        val result = (columnDescription, columnIdentifier)
        Right(result)
    }
  }


  private def makeErrorMessage(childErrorMessage: ErrorMessage) :
  Either[ErrorMessage,(FixedColumnDescription, FixedColumnIdentifier)] = {
    val errorMessage = UnrecoverableError(
      "creating a column identifier",
      "encountered a problem below",
      "solve the problems below",
      List(childErrorMessage)
    )
    Left(errorMessage)
  }
}