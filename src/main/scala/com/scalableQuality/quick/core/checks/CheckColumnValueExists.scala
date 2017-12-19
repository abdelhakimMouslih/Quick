package com.scalableQuality.quick.core.checks

class CheckColumnValueExists(
    shouldCheck: Boolean
) extends Check {
  override protected def executeCheck(value: String): Boolean = true
  override protected val defaultCheckResult: Boolean = !shouldCheck
}

object CheckColumnValueExists {
  def apply(
      shouldCheck: Boolean
  ): CheckColumnValueExists = new CheckColumnValueExists(shouldCheck)
}
