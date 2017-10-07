package com.scalableQuality.quick.surface.commandLineOptions


// quick -d desc.xml -i loadCardHolder -l label1,Label2 file1 file2
case class CommandLineInput(
                                                        descriptionFile: String = "",
                                                        descriptionId: Option[String] = None,
                                                        leftFileLabel: Option[String] = None,
                                                        rightFileLabel: Option[String] = None,
                                                        leftFile: String = "",
                                                        rightFile: String = ""
                                                      ) {
  def addLabel(label: String): CommandLineInput = leftFileLabel match {
    case None =>
      this.copy(leftFileLabel = Some(label))
    case _ =>
      this.copy(rightFileLabel = Some(label))
  }
  def addFile(file:String): CommandLineInput = leftFile match {
    case "" =>
      this.copy(leftFile = file)
    case _ =>
      this.copy(rightFile = file)
  }
}