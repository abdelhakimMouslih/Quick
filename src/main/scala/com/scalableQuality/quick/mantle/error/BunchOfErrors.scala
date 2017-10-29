package com.scalableQuality.quick.mantle.error

case class BunchOfErrors(
                   errors: List[UnrecoverableError]
                   ) extends UnrecoverableError {
  override def toString: String = concatenateEncounteredErrors(errors)

  private def concatenateEncounteredErrors(encounteredErrors: List[UnrecoverableError]): String = {
    val endOfLine = BunchOfErrors.endOfLine
    encounteredErrors.mkString(s"${endOfLine}and${endOfLine}")
  }

}

object BunchOfErrors {
  private[BunchOfErrors] val endOfLine = System.getProperty("line.separator")
}