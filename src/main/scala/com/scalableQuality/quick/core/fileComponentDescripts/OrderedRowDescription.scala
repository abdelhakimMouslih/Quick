package com.scalableQuality.quick.core.fileComponentDescripts


import com.scalableQuality.quick.core.others.{ColumnUsageStages, MatchingStage, ValidationStage}
import com.scalableQuality.quick.mantle.parsing.RawRow
import java.security.MessageDigest

import com.scalableQuality.quick.core.Reporting.ComparisonBetweenTwoColumns

import scala.annotation.tailrec

case class OrderedRowDescription(
                             val columnsDescriptions: List[ColumnDescription],
                             val label: String
                           ) {

  def keepOnlyColumnsDescriptionsUsedIn(
              columnUsages: ColumnUsageStages*
              ): OrderedRowDescription = {
    val keptColumnDescriptions = for {
      colDesc <- columnsDescriptions
      if colDesc.shouldUseDuring(columnUsages:_*)
    } yield colDesc
    OrderedRowDescription(keptColumnDescriptions, this.label )
  }

  def compare(
               leftFileRawRow: Option[RawRow],
               rightFileRawRow: Option[RawRow]
             ): List[ComparisonBetweenTwoColumns] = for {
    colDesc <- columnsDescriptions
  } yield ComparisonBetweenTwoColumns(
    colDesc,
    leftFileRawRow.flatMap(colDesc.columnValue),
    leftFileRawRow.flatMap(colDesc.columnValue),
    leftFileRawRow.flatMap(colDesc.comparisonValue) == leftFileRawRow.flatMap(colDesc.comparisonValue)
  )

  // validationSignatureOf and matchingSignatureOf functions return
  // List[Byte] instead of List[Option[String]] to avoid huge ram usage
  def validationSignatureOf(
                             row: RawRow
                           ): List[Option[List[Byte]]] = {
    val validationValue = validationValuesOf(row)
    OrderedRowDescription.getSignature(validationValue)
  }

  def matchingSignatureOf(
                           row: RawRow
                         ): List[Option[List[Byte]]] = {
    val matchingValue = matchingValuesOf(row)
    OrderedRowDescription.getSignature(matchingValue)
  }


  def isMatchable: Boolean = columnsDescriptions.collectFirst {
    case col if col shouldUseDuring MatchingStage => true
  }.getOrElse(false)

  private def validationValuesOf(
                               row: RawRow
                               ): List[Option[String]]  = for (
    colDesc <- this.columnsDescriptions
      if colDesc shouldUseDuring ValidationStage
  ) yield colDesc.comparisonValue(row)



  private def matchingValuesOf(
                           row: RawRow
                           ): List[Option[String]] = for (
      colDesc <- this.columnsDescriptions
      if colDesc shouldUseDuring MatchingStage
    ) yield colDesc.comparisonValue(row)


}

object OrderedRowDescription {
  /*def apply(
             columnsDescriptions: List[ColumnDescription],
             label: String
           ): OrderedRowDescription = new OrderedRowDescription(columnsDescriptions, label)

*/
  // the sha1Digest object, shaSum function and getSignature function are
  // in the companion object to avoid creating an sha1Digest object for
  // each OrderedRow instance
  private val sha1Digest = MessageDigest.getInstance("SHA1")
  private def sha1Sum(bytes: Array[Byte]): List[Byte] = {
    sha1Digest.reset()
    sha1Digest.update(bytes)
    sha1Digest.digest().toList
  }


  // to avoid considering a row with absent columns values equivalent
  // to a row with empty columns the getSignature function will
  // return a list consisting of Some[List[Byte]] to represent the
  // resulted hash of concatenated successive column values
  // and None to represented the absent column

  // the getSignature function will append the column value's length at to it
  // to avoid generating the same hash for the two List[Option[String]]
  // below List(Some("AAA"),Some("AA")) and List(Some("AA"),Some("AAA"))

  private[fileComponentDescripts] def getSignature(
                                                columns: List[Option[String]]
                                              ): List[Option[List[Byte]]] = {
    def appendColumnValue(
                           stringBuilderOption: Option[StringBuilder],
                           columnValue: String
                         ): Option[StringBuilder] = stringBuilderOption match {
      case None =>
        val newStringBuilder = new StringBuilder(columnValue)
        newStringBuilder append columnValue.length.toString
        Some(newStringBuilder)
      case Some(stringBuilder) =>
        stringBuilder append columnValue
        stringBuilder append columnValue.length.toString
        Some(stringBuilder)
    }
    @tailrec def loop(
                       columns: List[Option[String]],
                       concatenatedPreviousColumnValuesOpt: Option[StringBuilder],
                       digestedBytes: List[Option[List[Byte]]]
                     ): List[Option[List[Byte]]] = columns match {
      case Nil =>
        concatenatedPreviousColumnValuesOpt match {
          case None =>
            digestedBytes.reverse
          case Some(columnValue) =>
            val signatureOfPreviousColumnValues = sha1Sum(columnValue.toString.getBytes)
            val finalDigestedBytes = Some(signatureOfPreviousColumnValues)
            (finalDigestedBytes :: digestedBytes).reverse
        }
      case Some(columnValue)::restOfColumnValues =>
        val concatenatedStrings: Option[StringBuilder] = appendColumnValue(concatenatedPreviousColumnValuesOpt, columnValue)
        loop(restOfColumnValues, concatenatedStrings, digestedBytes)
      case None::restOfColumnValues =>
        concatenatedPreviousColumnValuesOpt match {
          case None =>
            loop(restOfColumnValues, concatenatedPreviousColumnValuesOpt, None::digestedBytes)
          case Some(concatenatedPreviousColumnValues) =>
            val signatureOfPreviousColumnValues = sha1Sum(concatenatedPreviousColumnValues.toString.getBytes)
            loop(restOfColumnValues,
              None,
              None :: Some(signatureOfPreviousColumnValues) :: digestedBytes
            )
        }
    }
    loop(columns, None, Nil)
  }
}