package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.core.fileComponentDescriptions.FixedColumnDescription
import com.scalableQuality.quick.mantle.constructFromXml.XMLHelperFunctions
import com.scalableQuality.quick.mantle.error.{
  BunchOfErrors,
  UnrecoverableError
}
import com.scalableQuality.quick.mantle.parsing.errorMessages.FixedColumnIdentifierErrorMessages

import scala.xml.MetaData

class FixedColumnIdentifier(
    matchAgainst: MatchAgainst,
    columnDescription: FixedColumnDescription
) {
  def apply(row: RawRow): Boolean = {
    val extractedColumn = columnDescription.columnValue(row)
    extractedColumn
      .map(matchAgainst(_))
      .getOrElse(RowToRowDescriptionMatcher.defaultIdentificationResult)
  }
}

object FixedColumnIdentifier {

  def apply(
      matchAgainst: MatchAgainst,
      columnDescription: FixedColumnDescription
  ): FixedColumnIdentifier =
    new FixedColumnIdentifier(matchAgainst, columnDescription)

  def apply(elemMetaData: MetaData)
    : Either[UnrecoverableError,
             (FixedColumnDescription, FixedColumnIdentifier)] = {
    val unknownAttributesErrors = XMLHelperFunctions.collectUnknownAttributes(
      listOfAttributesKeys,
      elemMetaData)
    unknownAttributesErrors match {
      case Nil =>
        val matchAgainstEither = MatchAgainst(elemMetaData)

        val metaDataWithoutMatchAgainst = XMLHelperFunctions.removeAttributes(
          elemMetaData,
          MatchAgainst.listOfAttributesKeys)
        val columnDescriptionEither = FixedColumnDescription(
          metaDataWithoutMatchAgainst)

        validateAttributeValues(columnDescriptionEither, matchAgainstEither) match {
          case Right((columnDescription, matchAgainst)) =>
            val columnIdentifier =
              FixedColumnIdentifier(matchAgainst, columnDescription)
            val result = (columnDescription, columnIdentifier)
            Right(result)

          case Left(errorMessages) =>
            FixedColumnIdentifierErrorMessages.invalidAttributes(errorMessages)
        }
      case _ =>
        val bunchOfErrors = BunchOfErrors(unknownAttributesErrors)
        FixedColumnIdentifierErrorMessages.invalidAttributes(bunchOfErrors)
    }

  }

  private def validateAttributeValues(
      columnDescriptionEither: Either[UnrecoverableError,
                                      FixedColumnDescription],
      matchAgainstEither: Either[UnrecoverableError, MatchAgainst]
  ): Either[List[UnrecoverableError], (FixedColumnDescription, MatchAgainst)] =
    (columnDescriptionEither, matchAgainstEither) match {
      case (Right(columnDescription), Right(matchAgainst)) =>
        val classParameters = (columnDescription, matchAgainst)
        Right(classParameters)

      case _ =>
        UnrecoverableError.collectAllErrors(columnDescriptionEither,
                                            matchAgainstEither)
    }
  val listOfAttributesKeys = FixedColumnDescription.listOfAttributesKeys ::: MatchAgainst.listOfAttributesKeys
}
