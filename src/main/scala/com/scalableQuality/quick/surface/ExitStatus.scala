package com.scalableQuality.quick.surface

trait ExitStatus {
  val value: Int
}

case object FilesAreIdentical extends ExitStatus {
  override val value: Int = 0
}

case object InterruptedByAnError extends ExitStatus {
  override val value: Int = 1
}

case object FilesAreDifferent extends ExitStatus {
  override val value: Int = 2
}
