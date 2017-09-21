package com.scalableQuality.quick.core.fileProcessing

import com.scalableQuality.quick.core.fileComponentDescripts.OrderedRowDescription
import com.scalableQuality.quick.mantle.parsing.RawRow

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object MatchingProcess {

  def apply(
           orderedRowDescription: OrderedRowDescription,
           leftFile: List[RawRow],
           rightFile: List[RawRow]
           ): List[(Option[RawRow], Option[RawRow])] = if (orderedRowDescription.isMatchable) {
    val leftFileAsHashMap = buildValidationHashMap(orderedRowDescription, leftFile)
    matchRows(
      leftFileAsHashMap,
      orderedRowDescription,
      rightFile,
      Nil
    )
  } else {
    val unmatchableLeftRows = leftFile.map(row => (Some(row), None))
    val unmatchableRightRows = rightFile.map(row => (None, Some(row)))
    unmatchableLeftRows ::: unmatchableRightRows
  }

  private type matchingSignature =  immutable.List[Option[List[Byte]]]
  private type matchingSignatureHashMap = mutable.HashMap[matchingSignature, mutable.Set[RawRow]] with mutable.MultiMap[matchingSignature, RawRow]

  private def buildValidationHashMap(
                                      orderedRowDescription: OrderedRowDescription,
                                      file: List[RawRow]
                                    ): matchingSignatureHashMap = {
    val validationHashMap = new mutable.HashMap[matchingSignature, mutable.Set[RawRow]] with mutable.MultiMap[matchingSignature, RawRow]
    file.foreach{
      row =>
        val matchingSignature = orderedRowDescription.validationSignatureOf(row)
        validationHashMap.addBinding(matchingSignature, row)
    }
    validationHashMap
  }

  @tailrec private def matchRows(
                                leftFileAsHashMap: matchingSignatureHashMap,
                                orderedRowDescription: OrderedRowDescription,
                                rightFile: List[RawRow],
                                matchingResult: List[(Option[RawRow], Option[RawRow])]
                                ): List[(Option[RawRow], Option[RawRow])] = rightFile match {
    case Nil =>
      val unmatchedLeftRows = for {
        tupleOfKeyAndRowSet <- leftFileAsHashMap
        rowSet <- tupleOfKeyAndRowSet._2
      } yield (Some(rowSet),None)
      (unmatchedLeftRows.toList) ::: matchingResult


    case _ if (leftFileAsHashMap.isEmpty) =>
      val unmatchedRightRows = rightFile.map(row => (None, Some(row)))
      unmatchedRightRows ::: matchingResult


    case rightRow :: restOfRightRows =>
      val rightRowMatchingSignature = orderedRowDescription.matchingSignatureOf(rightRow)
      val matchedLeftRowOpt = leftFileAsHashMap.get(rightRowMatchingSignature).flatMap(_.headOption)
      matchedLeftRowOpt match {
        case None =>
          matchRows(
            leftFileAsHashMap,
            orderedRowDescription,
            restOfRightRows,
            (None, Some(rightRow)) :: matchingResult
          )

        case Some(matchedLeftRow) =>
          leftFileAsHashMap.removeBinding(rightRowMatchingSignature, matchedLeftRow)
          matchRows(
            leftFileAsHashMap,
            orderedRowDescription,
            restOfRightRows,
            (matchedLeftRowOpt,Some(rightRow)) :: matchingResult
          )
      }
  }
}
