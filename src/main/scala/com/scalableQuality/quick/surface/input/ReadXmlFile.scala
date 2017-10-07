package com.scalableQuality.quick.surface.input

import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, XML}

object ReadXmlFile {
  def apply(filePath: String): Either[ErrorMessage, Elem] = Try(readXmlFile(filePath)) match {
    case Success(elem) => Right(elem)

    case Failure(error) =>
      val errorMessage = UnrecoverableError(
        s"reading xml file ${filePath}",
        "connot read it",
        error.toString
      )
      Left(errorMessage)
  }
  private def readXmlFile(filePath: String): Elem = XML.loadFile(filePath)
}
