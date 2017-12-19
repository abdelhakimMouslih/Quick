package com.scalableQuality.quick.core.checks

trait Check {
  protected def executeCheck(value: String): Boolean
  protected val defaultCheckResult = Check.defaultColumnCheckResult

  def apply(value: Option[String]): Boolean =
    value.map(executeCheck(_)).getOrElse(defaultCheckResult)
}

object Check {
  val defaultColumnCheckResult = false
  val noChecksWereExecutedDefaultResult = true
}