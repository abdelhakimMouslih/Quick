package com.scalableQuality.quick.core.checks

trait Check {
  protected def executeCheck(value: String): Boolean
  protected val defaultCheckResult = false

  def apply(value: Option[String]): Boolean =
    value.map(executeCheck(_)).getOrElse(defaultCheckResult)

}
