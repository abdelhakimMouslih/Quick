package com.scalableQuality.quick.mantle.error

class EncounteredError(
                        whileDoing : String,
                        stoppedBecause : String,
                        toSolveTheProblem : String
                      ) extends UnrecoverableError {
  override def toString: String =
    s"""while ${whileDoing}
       |process is interrupted because ${stoppedBecause}
       |to solve the problem ${toSolveTheProblem}
     """.stripMargin
}


object EncounteredError {
  def apply(
             whileDoing: String,
             stoppedBecause: String,
             toSolveTheProblem: String
           ): EncounteredError = new EncounteredError(whileDoing, stoppedBecause, toSolveTheProblem)
}