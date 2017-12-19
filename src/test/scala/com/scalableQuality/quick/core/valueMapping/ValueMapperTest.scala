package com.scalableQuality.quick.core.valueMapping

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class ValueMapperTest
    extends FlatSpec
    with Matchers
    with TableDrivenPropertyChecks {

  "ValueMapper.apply(MetaData)" should "default to false if no attributes are present" in {
    val columnDescription = <ColumnDescription/>
    val valueMapperEither = ValueMapper(columnDescription.attributes)
    val inputString = Some(" PraiseTheSun ")
    val expectedString = Some(" PraiseTheSun ")
    valueMapperEither match {
      case Left(_) =>
        fail

      case Right(valueMapper) =>
        valueMapper(inputString) shouldBe expectedString
    }
  }

  it should "should handle upper case boolean values" in {
    val columnDescription =
      <ColumnDescription trimValue="TRUE" ignoreValueCase="TRUE" />
    val valueMapperEither = ValueMapper(columnDescription.attributes)
    val inputString = Some(" PraiseTheSun ")
    val expectedString = Some("PRAISETHESUN")
    valueMapperEither match {
      case Left(_) =>
        fail

      case Right(valueMapper) =>
        valueMapper(inputString) shouldBe expectedString
    }
  }

  val invalidBooleans = Table(
    ("trimValue", "ignoreValueCase"),
    ("bla", "true"),
    ("false", "bla")
  )
  it should "return Left[ErrorMessage, ValueMapper] if any of attributes are invalid booleans" in
    forAll(invalidBooleans) { (trimValue: String, ignoreValueCase: String) =>
      val columnDescription = <ColumnDescription
      trimValue={trimValue}
      ignoreValueCase={ignoreValueCase}
      />
      val valueMapperEither = ValueMapper(columnDescription.attributes)
      valueMapperEither shouldBe a[Left[_, _]]
    }

  "ValueMapper.apply(Option[String])" should "returned a string with no trailing or leading space chars when trimValue is true " in {
    val columnDescription = <ColumnDescription trimValue="true" />
    val valueMapperEither = ValueMapper(columnDescription.attributes)
    val inputString = Some(" Praise The Sun ")
    val expectedString = Some("Praise The Sun")
    valueMapperEither match {
      case Left(_) =>
        fail

      case Right(valueMapper) =>
        valueMapper(inputString) shouldBe expectedString
    }
  }

  it should "return an upper case string when ignoreValueCase is true " in {
    val columnDescription = <ColumnDescription ignoreValueCase="true" />
    val valueMapperEither = ValueMapper(columnDescription.attributes)
    val inputString = Some(" PraiseTheSun ")
    val expectedString = Some(" PRAISETHESUN ")
    valueMapperEither match {
      case Left(_) =>
        fail

      case Right(valueMapper) =>
        valueMapper(inputString) shouldBe expectedString
    }
  }

  it should "return an upper case string with no leading and trailing space when ignoreValueCase and trimValue are true " in {
    val columnDescription =
      <ColumnDescription trimValue="true" ignoreValueCase="true" />
    val valueMapperEither = ValueMapper(columnDescription.attributes)
    val inputString = Some(" Praise The Sun ")
    val expectedString = Some("PRAISE THE SUN")
    valueMapperEither match {
      case Left(_) =>
        fail

      case Right(valueMapper) =>
        valueMapper(inputString) shouldBe expectedString
    }
  }

}
