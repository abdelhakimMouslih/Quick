package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts.FixedColumnDescription
import com.scalableQuality.quick.core.others.MatchAgainst
import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.mantle.parsing.errorMessages.FixedColumnIdentifierErrorMessages

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


  def apply(elemMetaData: MetaData): Either[UnrecoverableError,(FixedColumnDescription, FixedColumnIdentifier)] = {

    val columnDescriptionEither = FixedColumnDescription(elemMetaData)

    val matchAgainstEither = MatchAgainst(elemMetaData)

    validateAttributeValues(columnDescriptionEither, matchAgainstEither) match {
      case Right((columnDescription, matchAgainst)) =>
        val columnIdentifier = FixedColumnIdentifier(matchAgainst, columnDescription)
        val result = (columnDescription, columnIdentifier)
        Right(result)

      case Left(errorMessages) =>
        FixedColumnIdentifierErrorMessages.invalidAttributes(errorMessages)
    }
  }

  private def validateAttributeValues(
                                       columnDescriptionEither: Either[UnrecoverableError,FixedColumnDescription],
                                       matchAgainstEither: Either[UnrecoverableError, MatchAgainst]
                                     ): Either[List[UnrecoverableError], (FixedColumnDescription, MatchAgainst)] =
    (columnDescriptionEither, matchAgainstEither) match {
      case (Right(columnDescription),Right(matchAgainst)) =>
        val classParameters = (columnDescription, matchAgainst)
        Right(classParameters)

      case _ =>
        UnrecoverableError.collectAllErrors(columnDescriptionEither, matchAgainstEither)
    }

}