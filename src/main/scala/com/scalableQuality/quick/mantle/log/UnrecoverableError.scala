package com.scalableQuality.quick.mantle.log

class UnrecoverableError(
                        override val toString: String
                        ) extends ErrorMessage

object UnrecoverableError {
  def apply(
             whileDoing: String,
             message: String,
             preventiveAction: String
           ) : UnrecoverableError = apply(whileDoing,message,preventiveAction, Nil)

  def apply(
             whileDoing: String,
             message: String,
             preventiveAction: String,
             errorTrace: List[ErrorMessage]
           ) : UnrecoverableError = {
    val header = banner

    val body =
    s"""${formatLabel("while")} $whileDoing
       |${formatLabel("message")} $message
       |${formatLabel("preventive action")} $preventiveAction""".stripMargin

    val footer =
    s"""${formatLabel("error trace")}
       |${eachRowStarsWith(formatErrorTrace(errorTrace), identation)}
     """.stripMargin

    val toString: String =
    s"""${header}
       |${eachRowStarsWith(body, identation)}
       |${eachRowStarsWith(footer, identation)}
     """.stripMargin

    new UnrecoverableError(toString)
  }

  protected val banner: String = "[ unrecoverable error ]"
  private val identation = "    "
  private def eachRowStarsWith(rows: String, start: String): String = rows.replaceAll("(?m)^", start)
  private def formatLabel(label: String): String = "%-18s :".format(label)
  private def formatErrorTrace(errTrace: List[ErrorMessage]): String = errTrace.mkString("\n")
}