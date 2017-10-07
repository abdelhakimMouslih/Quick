package com.scalableQuality.quick.surface.output

object WriteToStdout {
  def apply(lines: String*): Unit = lines.foreach(Console.out.println)
}
