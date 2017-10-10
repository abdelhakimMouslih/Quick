package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.mantle.log.ErrorMessage
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.xml.MetaData

trait ColumnPosition {
  override def toString: String
  def extractColumnValue(row: RawRow): Option[String]
}

object ColumnPosition {
  def apply(metaData: MetaData): Either[ErrorMessage, ColumnPosition] = FixedPosition(metaData)
}