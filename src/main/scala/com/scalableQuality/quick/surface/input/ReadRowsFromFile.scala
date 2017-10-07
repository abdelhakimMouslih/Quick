package com.scalableQuality.quick.surface.input

import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.io.Source
import scala.util.{Failure, Success, Try}

object ReadRowsFromFile {

  def apply(filePath: String): Either[ErrorMessage, () => List[RawRow]] = Try(getIteratorOverRows(filePath)) match {
    case Success(iteratorOverRows) =>
      Right(reader(iteratorOverRows))
    case Failure(error) =>
      val unrecoverableError = UnrecoverableError (
        s"reading file ${filePath}",
        "cannot read the file",
        error.toString
      )
      Left(unrecoverableError)
  }


  private def getIteratorOverRows(filePath: String): Iterator[String] = Source.fromFile(filePath).getLines()

  private def reader(iteratorOverRows: Iterator[String] ): () => List[RawRow] = () => {
    val fileRows = iteratorOverRows.toList
    var rowNumber: Int = RawRow.firstIndex - 1
    for(
      row <- fileRows
    ) yield {
      rowNumber += 1
      RawRow(row, rowNumber)
    }
  }

}
