package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts.FixedColumnDescription
import com.scalableQuality.quick.core.others.{MatchAgainst, ValueMapper}
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.xml.MetaData

class ColumnIdentifier(
                        matchAgainst: MatchAgainst,
                        columnDescription: FixedColumnDescription
                      ) {
  def apply(row: RawRow): Boolean = {
    val extractedColumn = columnDescription.columnValue(row)
    extractedColumn.map(matchAgainst(_)).getOrElse(ColumnIdentifier.defaultValidationResult)
  }
}

object ColumnIdentifier {
  private [ColumnIdentifier] val defaultValidationResult = false

  def apply(
             matchAgainst: MatchAgainst,
             columnDescription: FixedColumnDescription
           ): ColumnIdentifier = new ColumnIdentifier(matchAgainst, columnDescription)


  def apply(elemMetaData: MetaData): Either[ErrorMessage,(FixedColumnDescription, ColumnIdentifier)] = {
    val columnDescriptionEither = FixedColumnDescription(elemMetaData)
    val matchAgainstEither = MatchAgainst(elemMetaData)
    (columnDescriptionEither, matchAgainstEither) match {
      case (Left(errorMessage), _) =>
        makeErrorMessage(errorMessage)
      case (_, Left(errorMessage)) =>
        makeErrorMessage(errorMessage)
      case (Right(columnDescription), Right(matchAgainst)) =>
        val columnIdentifier = ColumnIdentifier(matchAgainst, columnDescription)
        val result = (columnDescription, columnIdentifier)
        Right(result)
    }
  }


  private def makeErrorMessage(childErrorMessage: ErrorMessage) :
  Either[ErrorMessage,(FixedColumnDescription, ColumnIdentifier)] = {
    val errorMessage = UnrecoverableError(
      "creating a column identifier",
      "encountered a problem below",
      "solve the problems below",
      List(childErrorMessage)
    )
    Left(errorMessage)
  }
}