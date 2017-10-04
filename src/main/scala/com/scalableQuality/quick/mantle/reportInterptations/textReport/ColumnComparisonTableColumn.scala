package com.scalableQuality.quick.mantle.reportInterptations.textReport

import scala.annotation.tailrec


object ColumnComparisonTableColumn {

  def apply(valueOpt: Option[String], horizontalSize: Int): List[String] = valueOpt match {
    case None => List("")

    case Some(value) => ColumnComparisonTableColumn(value, horizontalSize)
  }

  def apply(value: String, horizontalSize:Int): List[String] = divide(value, horizontalSize, Nil)


  @tailrec def divide(
                       string: String,
                       substringLength: Int,
                       accumulator: List[String]
                     ): List[String] = if (string.length <= substringLength) {
    (string::accumulator).reverse
  } else {
    val substring = string.substring(0,substringLength)
    val restOfString = string.substring(substringLength)
    divide(restOfString, substringLength, substring::accumulator)
  }

  val emptyColumn = ""
}
