package com.scalableQuality.quick.core.valueMapping

trait ValueMapperFunction {
  def apply(value: Option[String]): Option[String]
}