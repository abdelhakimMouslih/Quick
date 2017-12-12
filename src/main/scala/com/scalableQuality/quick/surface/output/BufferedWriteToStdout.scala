package com.scalableQuality.quick.surface.output

import java.io.BufferedOutputStream

object BufferedWriteToStdout {
  def apply(lines: List[() => List[String]]) = {
    val stdout = new BufferedOutputStream(System.out)
    lines.foreach(
      lines => lines().foreach(bufferLines(stdout, _))
    )
    stdout.flush()
  }
  private def bufferLines(bufferedOutputStream: BufferedOutputStream,
                          line: String): Unit = {
    bufferedOutputStream.write(line.getBytes)
    bufferedOutputStream.write(endOfLine.getBytes)
  }

  private val endOfLine = System.getProperty("line.separator")
}
