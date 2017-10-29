package com.scalableQuality.quick.mantle.error

case class BlindError(errorMessage: String) extends UnrecoverableError {
  override def toString: String = errorMessage
}
