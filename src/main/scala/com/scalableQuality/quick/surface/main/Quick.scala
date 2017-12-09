package com.scalableQuality.quick.surface.main

import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.mantle.labels.FileLabelsFromPaths
import com.scalableQuality.quick.mantle.parsing.GroupRowsByRowDescription
import com.scalableQuality.quick.mantle.reportInterptations.textReport.ValidationAndMatchingTextReport
import com.scalableQuality.quick.surface.commandLineOptions.CommandLineParser
import com.scalableQuality.quick.surface.input.{ReadRowsFromFile, ReadXmlFile}
import com.scalableQuality.quick.surface.output.{ExitWithStatus, WriteTextReportToStdout, WriteToStderr}

object Quick extends App{

  val quickStateOpt = CommandLineParser(args)
  quickStateOpt match {
    case Some(quickState) =>
      val leftFilePath = quickState.leftFile
      val rightFilePath = quickState.rightFile
      val fileDescriptionPath = quickState.descriptionFile

      val fileDescriptionId : Option[String] = quickState.descriptionId

      val (leftFileLabel, rightFileLabel) = FileLabelsFromPaths(quickState)

      val leftFilePathEither = ReadRowsFromFile(leftFilePath)
      val rightFilePathEither = ReadRowsFromFile(rightFilePath)
      val fileDescriptionPathEither = ReadXmlFile(fileDescriptionPath)

      (leftFilePathEither, rightFilePathEither, fileDescriptionPathEither) match {
        case (Right(leftFileRows), Right(rightFileRows),Right(fileDescriptionRootElem)) =>

          val rowToRowDescriptionMatcherEither = GroupRowsByRowDescription(
            fileDescriptionRootElem,
            fileDescriptionId,
            leftFileLabel,
            rightFileLabel
          )
          rowToRowDescriptionMatcherEither match {
            case Right(rowDescriptionMatcher) =>
              val validationAndMatchingProcesses =
                rowDescriptionMatcher.validateAndMatchTheseTwoFiles(leftFileRows(), rightFileRows())

              val validationAndMatchingReports = validationAndMatchingProcesses.validationAndMatchingReports

              val validationAndMatchingTextReports = validationAndMatchingReports.map(ValidationAndMatchingTextReport(_))

              validationAndMatchingTextReports.foreach(WriteTextReportToStdout(_))
              ExitWithStatus.exitWithReport(validationAndMatchingReports)
            case Left(errorMessage) =>
              WriteToStderr(errorMessage)
              ExitWithStatus.interruptedByAnError
          }

        case _ =>
          val errorMessages = UnrecoverableError.collectAllErrorsToList(leftFilePathEither, rightFilePathEither, fileDescriptionPathEither)
          errorMessages.foreach(WriteToStderr(_))
          ExitWithStatus.interruptedByAnError
      }
    case None =>
      ExitWithStatus.interruptedByAnError
  }

}
