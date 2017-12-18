package com.scalableQuality.quick.surface.input

import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.mantle.parsing.FileDescriptionElem
import com.scalableQuality.quick.surface.commandLineOptions.QuickState
import com.scalableQuality.quick.surface.input.errorMessages.ReadFromFileErrorMessages

import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, XML}

object ReadXmlFile {
  def apply(quickState: QuickState): Either[UnrecoverableError, FileDescriptionElem] =
    Try(readXmlFile(quickState.descriptionFile)) match {
      case Success(elem) =>
        val fileDescriptionElem = FileDescriptionElem(elem, quickState.descriptionFile, quickState.descriptionId)
        Right(fileDescriptionElem)
      case Failure(throwable) =>
        ReadFromFileErrorMessages.cannotReadFile(quickState.descriptionFile, throwable)
    }
  private def readXmlFile(filePath: String): Elem = XML.loadFile(filePath)
}
