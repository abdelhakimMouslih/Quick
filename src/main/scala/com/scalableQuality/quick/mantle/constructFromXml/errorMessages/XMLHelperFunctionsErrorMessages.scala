package com.scalableQuality.quick.mantle.constructFromXml.errorMessages

import com.scalableQuality.quick.mantle.error.BlindError

import scala.xml.Attribute

object XMLHelperFunctionsErrorMessages {
  def unknownAttributeError(attribute: Attribute) = BlindError(
    s"unknown or misspelled attribute found ${attribute.key}"
  )
}
