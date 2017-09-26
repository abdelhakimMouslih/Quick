package com.scalableQuality.quick.core.others

import org.scalatest._

class ValueMapperTest extends FlatSpec {

  "ValueMapper(Trim)" should "remove traiing and leading white space" in {
    val valueMapper = ValueMapper(Trim)
    val inputString = Some("  Shit  ")
    val expectedString = Some("Shit")
    assert(valueMapper(inputString) === expectedString)
  }
}
