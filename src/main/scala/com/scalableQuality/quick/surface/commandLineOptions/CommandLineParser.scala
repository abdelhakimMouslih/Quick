package com.scalableQuality.quick.surface.commandLineOptions

object CommandLineParser {
  private val quickName = "quick"
  private val quickVersion = "0.4"

  def apply(args: Array[String]): Option[QuickState] =
    new scopt.OptionParser[QuickState](quickName) {
      head(quickName, quickVersion)

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

      opt[Unit]('p', "parallel")
        .action { (_, config) =>
          config.copy(rowsProcessingPhaseExecution =
            QuickState.parallelRowsProcessingPhaseExecutionFunction)
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
