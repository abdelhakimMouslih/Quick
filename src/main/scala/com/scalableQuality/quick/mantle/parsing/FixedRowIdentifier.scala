package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.mantle.parsing.errorMessages.FixedRowIdentifierErrorMessages

class FixedRowIdentifier(
    columnsIdentifiers: List[FixedColumnIdentifier]
) extends RowIdentifier {
  def canIdentify(rawRow: RawRow): Boolean = columnsIdentifiers match {
    case Nil => RowToRowDescriptionMatcher.defaultIdentificationResult
    case columnIdentifier :: restOfColumnIdentifiers =>
      val canIdentifyTheFirstColumn = columnIdentifier(rawRow)
      restOfColumnIdentifiers.foldLeft(canIdentifyTheFirstColumn)(
        _ && _(rawRow))
  }
}

object FixedRowIdentifier {
  def apply(
      columnIdentifiers: List[FixedColumnIdentifier]
  ): Either[UnrecoverableError, FixedRowIdentifier] = columnIdentifiers match {
    case Nil =>
      FixedRowIdentifierErrorMessages.noColumnIdentifierIsProvided
    case _ =>
      Right(new FixedRowIdentifier(columnIdentifiers))
  }
}
