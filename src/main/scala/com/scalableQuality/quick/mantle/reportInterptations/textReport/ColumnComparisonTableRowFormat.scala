package com.scalableQuality.quick.mantle.reportInterptations.textReport

class ColumnComparisonTableRowFormat(
    format: ColumnComparisonTableRow => String
) {
  def apply(
      row: ColumnComparisonTableRow
  ): String = format(row)
}

object ColumnComparisonTableRowFormat {
  def apply(
      format: ColumnComparisonTableRow => String
  ): ColumnComparisonTableRowFormat = new ColumnComparisonTableRowFormat(format)

  def apply(
      borders: ColumnComparisonTableColumnsBorders,
      sizes: ColumnComparisonTableColumnSizes
  ): ColumnComparisonTableRowFormat = ColumnComparisonTableRowFormat(
    formatARow(borders, sizes, _)
  )

  private def formatARow(
      borders: ColumnComparisonTableColumnsBorders,
      sizes: ColumnComparisonTableColumnSizes,
      row: ColumnComparisonTableRow
  ): String = {
    val labelColumnFormat = rightAlignFormat(sizes.labelColumnSize)
    val positionColumnFormat = rightAlignFormat(sizes.positionColumnSize)
    val valueColumnFormat = rightAlignFormat(sizes.valueColumnSize)
    s"${borders.leftBorder}${labelColumnFormat}${borders.middleBorder}${positionColumnFormat}${borders.firstColumnBorder}${valueColumnFormat}${borders.secondColumnBorder}${valueColumnFormat}${borders.rightBorder}"
      .format(
        row.label,
        row.position,
        row.leftValue,
        row.rightValue
      )
  }

  private def rightAlignFormat(size: Int) = s"%-${size}s"
}
