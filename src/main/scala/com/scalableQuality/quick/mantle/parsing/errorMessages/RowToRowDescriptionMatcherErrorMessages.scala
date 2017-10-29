package com.scalableQuality.quick.mantle.parsing.errorMessages

import com.scalableQuality.quick.mantle.error._

import scala.xml.Elem

object RowToRowDescriptionMatcherErrorMessages {
  private val validatingFixedRowDescription = "validating FixedOrderedRowDescription attributes and child elements"

  private val invalidFixedRowDescriptionHeadLine = "Invalid FixedOrderedRowDescription"

  private val invalidDelimitedRowDescriptionHeadLine = "Invalid DelimitedOrderedRowDescription"

  private val validatingDelimitedRowDescription = "validating DelimitedOrderedRowDescription attributes and child elements"


  def unknownFixedRowDescriptionChildElem(elem: Elem) =
    unknownChildElem(validatingFixedRowDescription, elem)


  def unknownDelimitedRowDescriptionChildElem(elem: Elem) =
    unknownChildElem(validatingDelimitedRowDescription, elem)


  def unknownRowDescriptionElem(elem: Elem) = {
    val errorMessage = EncounteredError(
      "validating *RowDescription Elems",
      s"""${elem.label} is an unknown elem""",
      "two elem are supported for now, FixedOrderedRowDescription and DelimitedOrderedRowDescription"
    )
    Left(errorMessage)
  }

  private def unknownChildElem(doing: String, elem: Elem) = {
    val errorMessages = EncounteredError(
      doing,
      s"""${elem.label} is an unknown elem""",
      "two elem are supported for now, columnIdentifier and columnDescription"
    )
    Left(errorMessages)
  }

  def invalidFixedRowDescriptionChildElemOrAttribute(
                                                      errorMessages: UnrecoverableError*
                                                    ) =
    invalidChildElemOrAttribute(invalidFixedRowDescriptionHeadLine, errorMessages.toList)

  def invalidDelimitedRowDescriptionChildElemOrAttribute(
                                                          errorMessages: UnrecoverableError*
                                                        ) =
    invalidChildElemOrAttribute(invalidDelimitedRowDescriptionHeadLine, errorMessages.toList )

  private def invalidChildElemOrAttribute(
                                           doing: String,
                                           errorMessages: List[UnrecoverableError]
                                         )  = {
    val errorMessage = DependencyError(
      doing,
      errorMessages
    )
    Left(errorMessage)
  }
}
