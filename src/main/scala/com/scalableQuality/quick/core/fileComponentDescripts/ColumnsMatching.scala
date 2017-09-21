//package com.scalableQuality.quick.core.fileRepresentation

//import scala.annotation.tailrec
//import scala.collection.mutable.{HashMap, ListBuffer, MultiMap, Set}

//object ColumnsMatching {
//  def apply(leftFileRows:List[OrderedRowDescription], rightFileRows: List[OrderedRowDescription]): List[(Option[OrderedRowDescription], Option[OrderedRowDescription])] = {
//
//  @tailrec def loop(
//                     leftFileRowsMutableMultiMap: HashMap[List[Byte], Set[OrderedRowDescription]] with MultiMap[List[Byte], OrderedRowDescription],
//                     rightFileRows: List[OrderedRowDescription],
//                     matchedRows: List[(Option[OrderedRowDescription], Option[OrderedRowDescription])]
//                   ) : List[(Option[OrderedRowDescription], Option[OrderedRowDescription])] = rightFileRows match {
//    case Nil =>
//      val unmatchedLeftFileRows = ListBuffer[(Option[OrderedRowDescription], Option[OrderedRowDescription])]()
//      for {
//        keyRowsSet <- leftFileRowsMutableMultiMap
//        rowInSet <- keyRowsSet._2
//      } yield {
//        unmatchedLeftFileRows += ((Some(rowInSet), None))
//      }
//      (unmatchedLeftFileRows ++= matchedRows).toList
//    case _ if leftFileRowsMutableMultiMap.isEmpty =>
//      rightFileRows.map( row => (None, Some(row))) ::: matchedRows
//    case rightFileRow::restOfRightFileRows =>
//      val matchingSignature = rightFileRow.matchingSignature
//      val matchedLeftRowOpt = leftFileRowsMutableMultiMap.get(matchingSignature).flatMap(_.headOption)
//      matchedLeftRowOpt match {
//        case None =>
//          loop(leftFileRowsMutableMultiMap,restOfRightFileRows, (None, Some(rightFileRow)) :: matchedRows )
//        case Some(matchedLeftRow) =>
//          leftFileRowsMutableMultiMap.removeBinding(matchingSignature, matchedLeftRow)
//          loop(leftFileRowsMutableMultiMap, restOfRightFileRows, (Some(matchedLeftRow),Some(rightFileRow)) :: matchedRows)
//      }
//  }

    // separating rows with no columns descriptions for matching
    // these rows are marked as unmatchable as to avoid confusing reports
    //    val (leftFileRowsWithMatchingValues, leftFileRowsWithoutMatchingValues) = leftFileRows.partition(_.unmatchable)
    //    val (rightFileRowsWithMatchingValues, rightFileRowsWithoutMatchingValues) = rightFileRows.partition(_.unmatchable)
//val unmatchedRows = ListBuffer[(Option[OrderedRowDescription], Option[OrderedRowDescription])]()
//    unmatchedRows ++= leftFileRowsWithoutMatchingValues.map( row => (Some(row), None) )
//  unmatchedRows ++= rightFileRowsWithoutMatchingValues.map( row => (None, Some(row)) )
    //
    //    val leftFileRowsMutableMultiHashMap = {
//val newHashMap = new HashMap[List[Byte], Set[OrderedRowDescription]] with MultiMap[List[Byte], OrderedRowDescription]
//    leftFileRowsWithMatchingValues.foreach{
//      row => newHashMap.addBinding(row.matchingSignature, row)
//    }
//    newHashMap
//  }

//    unmatchedRows ++= loop(leftFileRowsMutableMultiHashMap,rightFileRowsWithMatchingValues, Nil )
//  println("finished")
//  unmatchedRows.toList
//}
//}
