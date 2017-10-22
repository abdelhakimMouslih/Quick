package com.scalableQuality.quick.mantle.parsing

trait RowIdentifier {
  def canIdentify(rawRow: RawRow): Boolean
}
