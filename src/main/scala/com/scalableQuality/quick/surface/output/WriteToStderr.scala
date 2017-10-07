package com.scalableQuality.quick.surface.output

object WriteToStderr {
  def apply(lines: Any*): Unit = lines.foreach(Console.err.println)
}
