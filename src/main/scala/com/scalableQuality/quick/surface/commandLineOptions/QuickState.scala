package com.scalableQuality.quick.surface.commandLineOptions

import com.scalableQuality.quick.core.fileComponentDescriptions.OrderedRowDescription
import com.scalableQuality.quick.core.fileProcessingPhase.{
  MatchRows,
  RowsProcessingPhase,
  ValidateAndMatchRows
}
import com.scalableQuality.quick.mantle.parsing.RawRow
import QuickState.{RowsProcessingPhaseConstructor, validateAndMatchRows}

// quick -d desc.xml -i loadCardHolder -l label1,Label2 file1 file2
case class QuickState(
    descriptionFile: String = "",
    descriptionId: Option[String] = None,
    leftFileLabel: Option[String] = None,
    rightFileLabel: Option[String] = None,
    leftFile: String = "",
    rightFile: String = "",
    rowsProcessingPhase: RowsProcessingPhaseConstructor = validateAndMatchRows,
    ignoreUnknownRows: Boolean = false
) {
  def addLabel(label: String): QuickState = leftFileLabel match {
    case None =>
      this.copy(leftFileLabel = Some(label))
    case _ =>
      this.copy(rightFileLabel = Some(label))
  }
  def addFile(file: String): QuickState = leftFile match {
    case "" =>
      this.copy(leftFile = file)
    case _ =>
      this.copy(rightFile = file)
  }
}

object QuickState {
  type RowsProcessingPhaseConstructor = (OrderedRowDescription,
                                         List[RawRow],
                                         List[RawRow],
                                         Option[String],
                                         Option[String]) => RowsProcessingPhase

  def validateAndMatchRows: RowsProcessingPhaseConstructor =
    ValidateAndMatchRows(_, _, _, _, _)
  def matchRows: RowsProcessingPhaseConstructor = MatchRows(_, _, _, _, _)
}
