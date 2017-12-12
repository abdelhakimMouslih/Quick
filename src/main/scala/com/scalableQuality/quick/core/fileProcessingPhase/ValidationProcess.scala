package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object ValidationProcess {

  def apply(
      orderedRowDescription: OrderedRowDescription,
      leftFileRows: List[RawRow],
      rightFileRows: List[RawRow]
  ): (List[RawRow], List[RawRow]) = {
    val leftFileAsHashMap =
      buildValidationHashMap(orderedRowDescription, leftFileRows)
    validate(leftFileAsHashMap, orderedRowDescription, rightFileRows, Nil)
  }

  private type validationSignature = immutable.List[Option[List[Byte]]]

  // since the validation is just looking up whether a row in the left file
  // and a row in the left file have the same validation signature,
  // a hashmap with the validation signature as the key, is used to enable efficient validation

  // a multimap trait is used to avoid processing multiple rows with the same signature as a single row
  private type validationSignatureHashMap = mutable.HashMap[
    validationSignature,
    mutable.Set[RawRow]] with mutable.MultiMap[validationSignature, RawRow]

  private def buildValidationHashMap(
      orderedRowDescription: OrderedRowDescription,
      file: List[RawRow]
  ): validationSignatureHashMap = {
    val validationHashMap =
      new mutable.HashMap[validationSignature, mutable.Set[RawRow]]
      with mutable.MultiMap[validationSignature, RawRow]
    file.foreach { row =>
      val validationSignature = orderedRowDescription.validationSignatureOf(row)
      validationHashMap.addBinding(validationSignature, row)
    }
    validationHashMap
  }
  @tailrec private def validate(
      leftFileAsHashMap: validationSignatureHashMap,
      rowDescription: OrderedRowDescription,
      rightFile: List[RawRow],
      rowsAbsentFromLeftFile: List[RawRow]
  ): (List[RawRow], List[RawRow]) = rightFile match {
    case Nil =>
      val rowsAbsentFromRightFile = for {
        tupleOfKeyAndRowSet <- leftFileAsHashMap
        rowSet <- tupleOfKeyAndRowSet._2
      } yield rowSet
      (rowsAbsentFromRightFile.toList, rowsAbsentFromLeftFile)

    case _ if (leftFileAsHashMap.isEmpty) =>
      (Nil, rightFile ::: rowsAbsentFromLeftFile)

    case rightRow :: restOfRightFile =>
      val rightRowValidationSignature =
        rowDescription.validationSignatureOf(rightRow)
      val equivalentLeftRowOpt =
        leftFileAsHashMap.get(rightRowValidationSignature).flatMap(_.headOption)
      equivalentLeftRowOpt match {
        case None =>
          validate(
            leftFileAsHashMap,
            rowDescription,
            restOfRightFile,
            rightRow :: rowsAbsentFromLeftFile
          )

        case Some(equivalentLeftRow) =>
          leftFileAsHashMap.removeBinding(rightRowValidationSignature,
                                          equivalentLeftRow)
          validate(
            leftFileAsHashMap,
            rowDescription,
            restOfRightFile,
            rowsAbsentFromLeftFile
          )
      }
  }
}
