package com.scalableQuality.quick.surface.main

import com.scalableQuality.quick.mantle.parsing.{RawRow, RowToRowDescriptionMatcher}
import com.scalableQuality.quick.mantle.reportInterptations.textReport.ValidationAndMatchingTextReport
import com.scalableQuality.quick.surface.commandLineOptions.{CommandLineInput, CommandLineParser}
import com.scalableQuality.quick.surface.input.{ReadRowsFromFile, ReadXmlFile}
import com.scalableQuality.quick.surface.output.{WriteTextReportToStdout, WriteToStderr}

object Quick extends App{

  val commandLineInputOpt = CommandLineParser(args)
  commandLineInputOpt match {
    case Some(commandLineInput) =>
      val leftFilePath = commandLineInput.leftFile
      val rightFilePath = commandLineInput.rightFile
      val fileDescriptionPath = commandLineInput.descriptionFile

      val fileDescriptionId : Option[String] = commandLineInput.descriptionId
      val leftFileLabel: Option[String] = commandLineInput.leftFileLabel
      val rightFileLabel: Option[String] = commandLineInput.rightFileLabel

      val leftFilePathEither = ReadRowsFromFile(leftFilePath)
      val rightFilePathEither = ReadRowsFromFile(rightFilePath)
      val fileDescriptionPathEither = ReadXmlFile(fileDescriptionPath)

      (leftFilePathEither, rightFilePathEither, fileDescriptionPathEither) match {
        case (Right(leftFileRows), Right(rightFileRows),Right(fileDescriptionRootElem)) =>

          val rowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(
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

            case Left(errorMessage) =>
              WriteToStderr(errorMessage)
          }

        case (Left(errorMessage),_,_) =>
          WriteToStderr(errorMessage)
        case (_,Left(errorMessage),_) =>
          WriteToStderr(errorMessage)
        case (_,_, Left(errorMessage)) =>
          WriteToStderr(errorMessage)
      }
    case None =>
      System.exit(1)
  }

}
