//package com.scalableQuality.quick.core.fileRepresentation

//import scala.annotation.tailrec
//import scala.collection.mutable.{HashMap, MultiMap, Set}

//object FileValidation {
//  def apply(leftFileRows: List[OrderedRowDescription], rightFileRows: List[OrderedRowDescription]):(List[OrderedRowDescription], List[OrderedRowDescription]) = {
//
    // since the validation is just looking up whether a row in the left file
    // and a row in the left file have the same validation signature,
    // a hashmap with the validation signature as the key, is used to enable efficient validation

    // a multimap trait is used to avoid counting multiple rows with the same signature as a single row
//    @tailrec def loop(
// leftFileRowsMutableMultiMap : HashMap[List[Byte], Set[OrderedRowDescription]] with MultiMap[List[Byte], OrderedRowDescription],
//                       rightFileRows: List[OrderedRowDescription],
//                       rightFileRowsAbsentFromLeftFileRows : List[OrderedRowDescription]
//                     ):(List[OrderedRowDescription],List[OrderedRowDescription]) = rightFileRows match {
//      case Nil =>
//        val leftFileRowsAbsentFromRightFileRows = for {
//          keyRowsSet <- leftFileRowsMutableMultiMap
//          rowInSet <- keyRowsSet._2
//        } yield rowInSet
//        (leftFileRowsAbsentFromRightFileRows.toList, rightFileRowsAbsentFromLeftFileRows)
//
//      case rightFileRow::rightFileRestOfRows if (leftFileRowsMutableMultiMap.isEmpty) =>
//        (Nil, rightFileRows:::rightFileRowsAbsentFromLeftFileRows )
//
//      case rightFileRow::rightFileRestOfRows =>
//
//        val rightRowValidationSignature = rightFileRow.validationSignature
//        val equivalentLeftRowOpt = leftFileRowsMutableMultiMap.get(rightRowValidationSignature).flatMap(_.headOption)
//        equivalentLeftRowOpt match {
//          case None =>
//            loop(leftFileRowsMutableMultiMap, rightFileRestOfRows, rightFileRow::rightFileRowsAbsentFromLeftFileRows)
//          case Some(equivalentLeftRow) =>
//            leftFileRowsMutableMultiMap.removeBinding(rightRowValidationSignature, equivalentLeftRow)
//            loop(leftFileRowsMutableMultiMap, rightFileRestOfRows, rightFileRowsAbsentFromLeftFileRows)
//        }
//    }
//
//    val leftFileMutableMultiMap = {
//     val multiMap = new HashMap[List[Byte], Set[OrderedRowDescription]] with MultiMap[List[Byte], OrderedRowDescription]
//    leftFileRows.foreach {
//      row => multiMap.addBinding(row.validationSignature, row)
//    }
//    multiMap
//  }
//
//  loop(leftFileMutableMultiMap, rightFileRows, Nil)
//}
//}
