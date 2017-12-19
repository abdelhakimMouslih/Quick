package com.scalableQuality.quick.mantle.reportInterptations.textReport

case class ColumnComparisonTableColumnsBorders(
    leftBorder: String,
    middleBorder: String,
    firstColumnBorder: String,
    secondColumnBorder: String,
    rightBorder: String
)

object ColumnComparisonTableColumnsBorders {
  def apply(
      leftBorder: String,
      middleBorder: String,
      rightBorder: String
  ): ColumnComparisonTableColumnsBorders =
    new ColumnComparisonTableColumnsBorders(leftBorder,
                                            middleBorder,
                                            middleBorder,
                                            middleBorder,
                                            rightBorder)
}
