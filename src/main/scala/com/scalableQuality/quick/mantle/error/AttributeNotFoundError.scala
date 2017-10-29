package com.scalableQuality.quick.mantle.error

case class AttributeNotFoundError(
                              whileDoing : String,
                              stoppedBecause : String,
                              toSolveTheProblem : String
                            ) extends EncounteredError(
  whileDoing,
  stoppedBecause,
  toSolveTheProblem
)