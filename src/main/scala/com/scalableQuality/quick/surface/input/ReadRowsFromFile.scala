package com.scalableQuality.quick.surface.input

import com.scalableQuality.quick.mantle.error.UnrecoverableError
import com.scalableQuality.quick.mantle.parsing.RawRow
import com.scalableQuality.quick.surface.input.errorMessages.ReadFromFileErrorMessages

import scala.io.Source
import scala.util.{Failure, Success, Try}

object ReadRowsFromFile {

  def apply(filePath: String): Either[UnrecoverableError, () => List[RawRow]] =
    Try(getIteratorOverRows(filePath)) match {
      case Success(iteratorOverRows) =>
        Right(reader(iteratorOverRows))
      case Failure(throwable) =>
        ReadFromFileErrorMessages.cannotReadFile(filePath, throwable)
    }

  private def getIteratorOverRows(filePath: String): Iterator[String] =
    Source.fromFile(filePath).getLines()

  private def reader(iteratorOverRows: Iterator[String]): () => List[RawRow] =
    () => {
      val fileRows = iteratorOverRows.toList
      var rowNumber: Int = RawRow.firstIndex - 1
      for (row <- fileRows) yield {
        rowNumber += 1
        RawRow(row, rowNumber)
      }
    }

}
