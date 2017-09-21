package com.scalableQuality.quick.core.others

object Trim extends ValueMapperFunction {
  override def apply(value: Option[String]) =  value.map(_.trim)
}
