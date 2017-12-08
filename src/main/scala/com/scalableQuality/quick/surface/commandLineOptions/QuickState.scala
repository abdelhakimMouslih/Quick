package com.scalableQuality.quick.surface.commandLineOptions


// quick -d desc.xml -i loadCardHolder -l label1,Label2 file1 file2
case class QuickState(
                                                        descriptionFile: String = "",
                                                        descriptionId: Option[String] = None,
                                                        leftFileLabel: Option[String] = None,
                                                        rightFileLabel: Option[String] = None,
                                                        leftFile: String = "",
                                                        rightFile: String = "",
                                                        multiJob: Boolean = false
                                                      ) {
  def addLabel(label: String): QuickState = leftFileLabel match {
    case None =>
      this.copy(leftFileLabel = Some(label))
    case _ =>
      this.copy(rightFileLabel = Some(label))
  }
  def addFile(file:String): QuickState = leftFile match {
    case "" =>
      this.copy(leftFile = file)
    case _ =>
      this.copy(rightFile = file)
  }
}