package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

class FixedRowIdentifier(
                          columnsIdentifiers: List[FixedColumnIdentifier]
                        ) extends RowIdentifier {
  def canIdentify(rawRow: RawRow): Boolean = columnsIdentifiers match {
    case Nil => RowToRowDescriptionMatcher.defaultIdentificationResult
    case columnIdentifier::restOfColumnIdentifiers =>
      val canIdentifyTheFirstColumn = columnIdentifier(rawRow)
      restOfColumnIdentifiers.foldLeft(canIdentifyTheFirstColumn)(_ && _(rawRow))
  }
}

object FixedRowIdentifier {
  def apply(
             columnIdentifiers: List[FixedColumnIdentifier]
           ): Either[ErrorMessage,FixedRowIdentifier] = columnIdentifiers match {
    case Nil =>
      val errorMessage = UnrecoverableError(
        "making fixedRowIdentifier",
        "no columnIdentifiers",
        "provide a column identifiers"
      )
      Left(errorMessage)
    case _ =>
      Right(new FixedRowIdentifier(columnIdentifiers))
  }
}
