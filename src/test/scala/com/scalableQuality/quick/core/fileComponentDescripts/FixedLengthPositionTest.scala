package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.mantle.buildFromXml.{ParameterValueNotFound, ValidParameterValueFound}
import com.scalableQuality.quick.mantle.log.{ErrorMessage, ErrorMessagePlaceHolder}
import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FixedLengthPositionTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {


  "FixedLengthPosition.apply" should
    "return Right[ErrorMessage,FixedLengthPosition] when ( startsAt > 0 && startsAt <= endsAt)" in forAll(maxDiscardedFactor(20.0)) {
    (startsAt: Int, endsAt: Int) => whenever(startsAt > 0 && startsAt <= endsAt ) {
      val columnDescriptionXmlElem = <ColumnDescription startsAt={startsAt.toString} endsAt={endsAt.toString} />
      val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a [Right[_,_]]
    }
  }

  it should "return Right[ErrorMessage,FixedLengthPosition] when ( startsAt > 0 && length > 0)" in forAll(maxDiscardedFactor(20.0)) {
    (startsAt: Int, length: Int) => whenever(startsAt > 0 && length > 0) {
      val columnDescriptionXmlElem = <ColumnDescription startsAt={startsAt.toString} length={length.toString} />
      val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a [Right[_,_]]
    }
  }


  it should "return Left[ErrorMessage,FixedLengthPosition] when (startsAt < 1 )" in forAll {
    (startsAt: Int) => whenever(startsAt < 1) {
      val columnDescriptionXmlElem = <ColumnDescription startsAt={startsAt.toString} endsAt="2" />
      val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a [Left[_,_]]
    }
  }

  it should "return Left[ErrorMessage,FixedLengthPosition] when (startsAt > endsAt )" in forAll {
    (endsAt: Int) => whenever( endsAt < Int.MaxValue ) {
      val startsAt = Int.MaxValue
      val columnDescriptionXmlElem = <ColumnDescription startsAt={startsAt.toString} endsAt={endsAt.toString} />
      val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a [Left[_,_]]
    }
  }

  it should "return Left[ErrorMessage,FixedLengthPosition] when (length < 1 )" in forAll {
    (length: Int) => whenever( length < 1 ) {
      val startsAt = 1
      val columnDescriptionXmlElem = <ColumnDescription startsAt={startsAt.toString} length={length.toString} />
      val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a [Left[_,_]]
    }
  }

  it should "return Left[ErrorMessage,FixedLengthPosition] when startsAt is an invalid int" in {
      val columnDescriptionXmlElem = <ColumnDescription startsAt="bla" endsAt ="3" length="3" />
      val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
      fixedLengthPositionTest shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage,FixedLengthPosition] when endsAt is an invalid int" in {
    val columnDescriptionXmlElem = <ColumnDescription startsAt="1" endsAt ="dude" length="3" />
    val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage,FixedLengthPosition] when length is an invalid int" in {
    val columnDescriptionXmlElem = <ColumnDescription startsAt="1" endsAt ="3" length="bib" />
    val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage,FixedLengthPosition] when startsAt is missing" in {
    val columnDescriptionXmlElem = <ColumnDescription endsAt ="3" length="3" />
    val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage,FixedLengthPosition] when endsAt and length are missing" in {
    val columnDescriptionXmlElem = <ColumnDescription startsAt="1" />
    val fixedLengthPositionTest= FixedLengthPosition(columnDescriptionXmlElem.attributes)
    fixedLengthPositionTest shouldBe a [Left[_,_]]
  }


  "FixedLengthPosition.extractColumnValue" should "return Some(subString) where the first char is at (startsAt - 1) and the last at (endsAt - 1)" in {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(7),
      ValidParameterValueFound(12),
      ParameterValueNotFound(ErrorMessagePlaceHolder)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = Some("TheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return Some(subString) where the first char is at (startsAt - 1) and have a length of length" in {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(1),
      ParameterValueNotFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(12)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = Some("PraiseTheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return Some(subString) one char when length is 1" in {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(1),
      ParameterValueNotFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(1)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = Some("P")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return None if the string ends before (startsAt-1)" in {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(13),
      ValidParameterValueFound(13),
      ParameterValueNotFound(ErrorMessagePlaceHolder)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = None
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return None if the string ends before (endsAt - 1)" in {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(1),
      ValidParameterValueFound(13),
      ParameterValueNotFound(ErrorMessagePlaceHolder)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = None
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }

  it should "return None if the string's length is less than length" in {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(1),
      ParameterValueNotFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(13)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = None
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fail
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) shouldBe expectedColumnValue
    }
  }
}