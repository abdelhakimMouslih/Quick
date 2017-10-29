package com.scalableQuality.quick.mantle.error

case class InvalidAttributeValueError (
                                   whileDoing : String,
                                   stoppedBecause : String,
                                   toSolveTheProblem : String
                                 ) extends EncounteredError(
  whileDoing,
  stoppedBecause,
  toSolveTheProblem
)