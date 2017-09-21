package com.scalableQuality.quick.core.others

trait ValueMapperFunction {
  def apply(value: Option[String]): Option[String]
}