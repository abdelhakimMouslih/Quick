package com.scalableQuality.quick.mantle.reportInterptations.textReport

case class ColumnComparisonTableRow(
    label: String,
    position: String,
    leftValue: String,
    rightValue: String
)

object ColumnComparisonTableRow {

  def apply(
      sizes: ColumnComparisonTableColumnSizes,
      label: String,
      position: String,
      leftValue: Option[String],
      rightValue: Option[String]
  ): List[ColumnComparisonTableRow] = {
    val labelColumns = ColumnComparisonTableColumn(label, sizes.labelColumnSize)
    val positionColumn =
      ColumnComparisonTableColumn(position, sizes.positionColumnSize)
    val leftValueColumn =
      ColumnComparisonTableColumn(leftValue, sizes.valueColumnSize)
    val rightValueColumn =
      ColumnComparisonTableColumn(rightValue, sizes.valueColumnSize)
    fitRows(
      labelColumns,
      positionColumn,
      leftValueColumn,
      rightValueColumn,
      Nil
    )
  }

  type Column = String
  type FittedColumns = (List[Column], List[Column], List[Column], List[Column])
  type Row = ColumnComparisonTableRow

  def destruct(listOfColumns: List[Column]): (Column, List[Column]) =
    listOfColumns match {
      case Nil                     => (ColumnComparisonTableColumn.emptyColumn, Nil)
      case column :: restOFColumns => (column, restOFColumns)
    }

  private def fitRows(
      fittedLabelColumn: List[Column],
      fittedPositionColumn: List[Column],
      fittedLeftValueColumn: List[Column],
      fittedRightValueColumn: List[Column],
      listOfRows: List[Row]
  ): List[Row] =
    (fittedLabelColumn,
     fittedPositionColumn,
     fittedLeftValueColumn,
     fittedRightValueColumn) match {
      case (Nil, Nil, Nil, Nil) =>
        listOfRows.reverse

      case (labelColumn :: Nil,
            positionColumn :: Nil,
            leftValueColumn :: Nil,
            rightValueColum :: Nil) =>
        val row = ColumnComparisonTableRow(labelColumn,
                                           positionColumn,
                                           leftValueColumn,
                                           rightValueColum)
        (row :: listOfRows).reverse
      case _ =>
        val (labelColumn, restOfLabelColumn) = destruct(fittedLabelColumn)
        val (positionColumn, restOfPositionColumn) = destruct(
          fittedPositionColumn)
        val (leftValueColumn, restOfLeftValueColumn) = destruct(
          fittedLeftValueColumn)
        val (rightValueColumn, restOfRightValueColumn) = destruct(
          fittedRightValueColumn)
        fitRows(
          restOfLabelColumn,
          restOfPositionColumn,
          restOfLeftValueColumn,
          restOfRightValueColumn,
          ColumnComparisonTableRow(labelColumn,
                                   positionColumn,
                                   leftValueColumn,
                                   rightValueColumn) :: listOfRows
        )
    }
}
