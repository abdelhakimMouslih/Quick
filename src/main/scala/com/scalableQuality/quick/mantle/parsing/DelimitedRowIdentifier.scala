package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.mantle.error._
import com.scalableQuality.quick.mantle.parsing.errorMessages.DelimitedRowIdentifierErrorMessages

class DelimitedRowIdentifier(
                              columnsIdentifiers: List[DelimitedColumnIdentifier],
                              delimiter: LiteralDelimiter
                            ) extends RowIdentifier {

  def canIdentify(rawRow: RawRow): Boolean = {
    val splitRow = this.delimiter.splitRow(rawRow)
    columnsIdentifiers match {
      case Nil => RowToRowDescriptionMatcher.defaultIdentificationResult
      case columnIdentifier::restOfColumnIdentifiers =>
        val canIdentifyTheFirstColumn = columnIdentifier(splitRow)
        restOfColumnIdentifiers.foldLeft(canIdentifyTheFirstColumn)(_ && _(splitRow))
    }
  }
}

object DelimitedRowIdentifier {
  def apply(
             columnsIdentifiers: List[DelimitedColumnIdentifier],
             delimiter: LiteralDelimiter
           ): Either[UnrecoverableError,DelimitedRowIdentifier] = columnsIdentifiers match {
    case Nil =>
      DelimitedRowIdentifierErrorMessages.noColumnIdentifierIsProvided
    case _ => Right(new DelimitedRowIdentifier(columnsIdentifiers, delimiter))
  }
}
