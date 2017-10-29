package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescripts.DelimitedColumnDescription
import com.scalableQuality.quick.core.others.MatchAgainst
import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.mantle.parsing.errorMessages.DelimitedColumnIdentifierErrorMessages
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

  def apply(elemMetaData: MetaData): Either[UnrecoverableError,(DelimitedColumnDescription, DelimitedColumnIdentifier)] = {

    val columnDescriptionEither = DelimitedColumnDescription(elemMetaData)

    val matchAgainstEither = MatchAgainst(elemMetaData)

    validateAttributeValues(columnDescriptionEither, matchAgainstEither) match {
      case Right((columnDescription, matchAgainst)) =>
        val columnIdentifier = DelimitedColumnIdentifier(matchAgainst, columnDescription)
        val result = (columnDescription, columnIdentifier)
        Right(result)

      case Left(errorMessages) =>
        DelimitedColumnIdentifierErrorMessages.invalidAttributes(errorMessages)
    }
  }

  private def validateAttributeValues(
                                     columnDescriptionEither: Either[UnrecoverableError,DelimitedColumnDescription],
                                     matchAgainstEither: Either[UnrecoverableError, MatchAgainst]
                                     ): Either[List[UnrecoverableError], (DelimitedColumnDescription, MatchAgainst)] =
    (columnDescriptionEither, matchAgainstEither) match {
      case (Right(columnDescription),Right(matchAgainst)) =>
        val classParameters = (columnDescription, matchAgainst)
        Right(classParameters)

      case _ =>
        UnrecoverableError.collectAllErrors(columnDescriptionEither, matchAgainstEither)
  }

}
