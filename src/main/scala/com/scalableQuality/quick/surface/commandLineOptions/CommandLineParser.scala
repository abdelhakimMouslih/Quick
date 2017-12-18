package com.scalableQuality.quick.surface.commandLineOptions

object CommandLineParser {
  private val quickName = "Quick\n"
  private val quickVersion = "\rrelease: 0.5\ncommit: 61e51a1"

  def apply(args: Array[String]): Option[QuickState] =
    new scopt.OptionParser[QuickState](quickName) {
      head(quickName, quickVersion)
      version("version")
      help("help")

      opt[String]('d', "description")
        .action { (optionValue, config) =>
          config.copy(descriptionFile = optionValue)
        }
        .required()

      opt[String]('i', "id")
        .action { (optionValue, config) =>
          config.copy(descriptionId = Some(optionValue))
        }
        .optional()

      opt[String]('l', "label")
        .action { (optionValue, config) =>
          config.addLabel(optionValue)
        }
        .optional()
        .maxOccurs(2)

      opt[Unit]('m', "matchOnly")
        .action { (_, config) =>
          config.copy(rowsProcessingPhase = QuickState.matchRows)
        }
        .optional()

      opt[Unit]('u', "unknown")
        .action { (_, config) =>
          config.copy(ignoreUnknownRows = true)
        }
        .optional()

      arg[String]("<leftFile> <rightFile>")
        .action { (optionValue, config) =>
          config.addFile(optionValue)
        }
        .required()
        .minOccurs(2)
        .maxOccurs(2)
    }.parse(args, QuickState())
}
