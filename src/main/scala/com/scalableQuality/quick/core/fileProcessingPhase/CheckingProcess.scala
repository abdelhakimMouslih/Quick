package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.fileComponentDescriptions.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.annotation.tailrec

object CheckingProcess {
  def apply(orderedRowDescription: OrderedRowDescription,
            rows: List[RawRow]): (List[RawRow], List[RawRow]) = {
    @tailrec def loop(
        orderedRowDescription: OrderedRowDescription,
        rows: List[RawRow],
        rowsPassedCheck: List[RawRow],
        rowsFailedCheck: List[RawRow]): (List[RawRow], List[RawRow]) =
      rows match {
        case Nil =>
          (rowsPassedCheck, rowsFailedCheck)
        case row :: restOfRows =>
          if (orderedRowDescription.check(row))
            loop(
              orderedRowDescription,
              restOfRows,
              row :: rowsPassedCheck,
              rowsFailedCheck
            )
          else
            loop(
              orderedRowDescription,
              restOfRows,
              rowsPassedCheck,
              row :: rowsFailedCheck
            )
      }
    loop(orderedRowDescription, rows, Nil, Nil)
  }
}
