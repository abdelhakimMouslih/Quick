package com.scalableQuality.quick.core.others

object ToUpperCase extends ValueMapperFunction {
  override def apply(value: Option[String]) =  value.map(_.toUpperCase)

  def apply(shouldInclude: Boolean): List[ValueMapperFunction] =
    if (shouldInclude) List(ToUpperCase) else Nil
}