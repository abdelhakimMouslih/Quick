package com.scalableQuality.quick.surface.input

import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.surface.input.errorMessages.ReadFromFileErrorMessages

import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, XML}

object ReadXmlFile {
  def apply(filePath: String): Either[UnrecoverableError, Elem] = Try(readXmlFile(filePath)) match {
    case Success(elem) => Right(elem)

    case Failure(throwable) =>
      ReadFromFileErrorMessages.cannotReadFile(filePath, throwable)
  }
  private def readXmlFile(filePath: String): Elem = XML.loadFile(filePath)
}
