package com.scalableQuality.quick.surface.commandLineOptions

object CommandLineParser {
  private val quickName = "Quick"
  private val quickVersion = "0.2"

  def apply(args: Array[String]): Option[CommandLineInput] =
    new scopt.OptionParser[CommandLineInput](quickName) {
      head(quickName, quickVersion)

      opt[String]('d', "description").action {
        (optionValue, config) =>
          config.copy(descriptionFile = optionValue)
      }.required()

      opt[String]('i', "id").action {
        (optionValue, config) =>
          config.copy(descriptionId = Some(optionValue))
      }.optional()

      opt[String]('l', "label").action {
        (optionValue, config) =>
          config.addLabel(optionValue)
      }.optional().maxOccurs(2)

      arg[String]("<leftFile> <rightFile>").action {
        (optionValue, config) =>
          config.addFile(optionValue)
      }.required().minOccurs(2).maxOccurs(2)
    }.parse(args, CommandLineInput())
}
