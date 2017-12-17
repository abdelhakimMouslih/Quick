package com.scalableQuality.quick.mantle.error

case class DependencyError(
    headLine: String,
    bunchOfErrors: BunchOfErrors
) extends UnrecoverableError {
  override def toString: String =
    s"""${headLine}
       |${indentAllLinesOf(bunchOfErrors)}
     """.stripMargin

  private def indentAllLinesOf(bunchOfErrors: BunchOfErrors): String =
    bunchOfErrors.toString.replaceAll("(?m)^", DependencyError.indentation)

}

object DependencyError {

  private[DependencyError] val indentation = " " * 4
  def apply(
      whileDoing: String,
      encounteredErrors: List[UnrecoverableError]
  ): DependencyError = {
    val bunchOfErrors = BunchOfErrors(encounteredErrors)
    new DependencyError(whileDoing, bunchOfErrors)
  }

  def apply(
      whileDoing: String,
      firstEncounteredError: UnrecoverableError,
      restOfEncounteredErrors: UnrecoverableError*
  ): DependencyError =
    DependencyError(whileDoing,
                    firstEncounteredError :: restOfEncounteredErrors.toList)
}
