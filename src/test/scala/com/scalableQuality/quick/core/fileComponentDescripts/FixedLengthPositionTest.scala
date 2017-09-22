package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.mantle.buildFromXml.{InvalidParameterValueFound, ParameterValueNotFound, ValidParameterValueFound}
import com.scalableQuality.quick.mantle.log.{ErrorMessage, ErrorMessagePlaceHolder}
import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalacheck.Prop

class FixedLengthPositionTest extends org.specs2.Specification with org.specs2.ScalaCheck {
  def is =
    s2"""
  FixedLengthPosition is only created when 0 < startsAt <= endsAt ${startsAtBetweenZeroAndStartsAtExpectation}
  FixedLengthPosition is only created when 0 < startsAt && 0 < length ${startsAtBiggerThanZeroAndLengthBiggerThanZeroExpectation}
  FixedLengthPosition is not created when startsAt is InvalidParameterValueFound ${startsAtIsInvalidParameterValueFoundExpectation}
  FixedLengthPosition is not created when startsAt is ParameterValueNotFound ${startsAtIsInvalidParameterValueFoundExpectation}
  FixedLengthPosition is not created when endsAt is InvalidParameterValueFound ${endsAtIsInvalidParameterValueFoundExpectation}
  FixedLengthPosition is not created when length is InvalidParameterValueFound ${lengthIsInvalidParameterValueFoundExpectation}
  FixedLengthPosition is not created when endsAt and length are ParameterValueNotFound ${endsAtAndLengthAreParameterValueNotFoundExpectation}
  extractColumnValue returns a substring starting at [startsAt - 1] and ending at (endsAt - 1) ${startsAtAndEndsAtExtractColumnValueExpectation}
  extractColumnValue returns a substring starting at [startsAt - 1] and ending at (length + startsAt - 2) ${startsAtAndLengthExtractColumnValueExpectation}
  extractColumnValue returns a None if the substring does not exist between startsAt and endsAt ${startsAtAndEndsAtExtractColumnReturnsNoneValueExpectation}
  extractColumnValue returns a None if the substring does not exist starting at startsAt with length length ${startsAtAndLengthExtractColumnReturnsNoneValueExpectation}
      """

  def startsAtBetweenZeroAndStartsAtExpectation = Prop.forAll {
    (startsAt: Int, endsAt: Int) =>
      val fixedLengthPositionTest: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition(
        ValidParameterValueFound(startsAt),
        ValidParameterValueFound(endsAt),
        ParameterValueNotFound(ErrorMessagePlaceHolder)
      )
      if(startsAt > 0 && startsAt <= endsAt)
        fixedLengthPositionTest must haveClass[Right[ErrorMessage,FixedLengthPosition]]
      else
        fixedLengthPositionTest must haveClass[Left[ErrorMessage,FixedLengthPosition]]
  }


  def startsAtBiggerThanZeroAndLengthBiggerThanZeroExpectation = Prop.forAll {
    (startsAt: Int, length: Int) =>
      val fixedLengthPositionTest: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition(
        ValidParameterValueFound(startsAt),
        ParameterValueNotFound(ErrorMessagePlaceHolder),
        ValidParameterValueFound(length)
      )
      if(startsAt > 0 && length > 0)
        fixedLengthPositionTest must haveClass[Right[ErrorMessage,FixedLengthPosition]]
      else
        fixedLengthPositionTest must haveClass[Left[ErrorMessage,FixedLengthPosition]]
  }

  def startsAtIsInvalidParameterValueFoundExpectation = {
    val fixedLengthPositionTest: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      InvalidParameterValueFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(10),
      ValidParameterValueFound(10)
    )
    fixedLengthPositionTest must haveClass[Left[ErrorMessage,FixedLengthPosition]]
  }

  def startsAtIsParameterValueNotFoundExpectation = {
    val fixedLengthPositionTest: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ParameterValueNotFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(10),
      ValidParameterValueFound(10)
    )
    fixedLengthPositionTest must haveClass[Left[ErrorMessage,FixedLengthPosition]]
  }

  def endsAtIsInvalidParameterValueFoundExpectation = {

    val fixedLengthPositionTest: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(10),
      InvalidParameterValueFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(20)
    )
    fixedLengthPositionTest must haveClass[Left[ErrorMessage,FixedLengthPosition]]
  }

  def lengthIsInvalidParameterValueFoundExpectation = {

    val fixedLengthPositionTest: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(10),
      ValidParameterValueFound(20),
      InvalidParameterValueFound(ErrorMessagePlaceHolder)
    )
    fixedLengthPositionTest must haveClass[Left[ErrorMessage,FixedLengthPosition]]
  }

  def endsAtAndLengthAreParameterValueNotFoundExpectation = {

    val fixedLengthPositionTest: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(10),
      ParameterValueNotFound(ErrorMessagePlaceHolder),
      ParameterValueNotFound(ErrorMessagePlaceHolder)
    )
    fixedLengthPositionTest must haveClass[Left[ErrorMessage,FixedLengthPosition]]
  }

  def startsAtAndEndsAtExtractColumnValueExpectation = {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(7),
      ValidParameterValueFound(12),
      ParameterValueNotFound(ErrorMessagePlaceHolder)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = Some("TheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fixedLengthPositionTestEither must haveClass[Right[ErrorMessage,FixedLengthPosition]]
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) mustEqual expectedColumnValue
    }
  }

  def startsAtAndLengthExtractColumnValueExpectation = {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(7),
      ParameterValueNotFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(13)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = Some("TheSun")
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fixedLengthPositionTestEither must haveClass[Right[ErrorMessage,FixedLengthPosition]]
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) mustEqual expectedColumnValue
    }
  }

  def startsAtAndEndsAtExtractColumnReturnsNoneValueExpectation = {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(7),
      ValidParameterValueFound(30),
      ParameterValueNotFound(ErrorMessagePlaceHolder)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = None
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fixedLengthPositionTestEither must haveClass[Right[ErrorMessage,FixedLengthPosition]]
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) mustEqual expectedColumnValue
    }
  }

  def startsAtAndLengthExtractColumnReturnsNoneValueExpectation = {
    val fixedLengthPositionTestEither: Either[ErrorMessage, FixedLengthPosition] = FixedLengthPosition (
      ValidParameterValueFound(7),
      ParameterValueNotFound(ErrorMessagePlaceHolder),
      ValidParameterValueFound(20)
    )
    val row = RawRow("PraiseTheSun",1)
    val expectedColumnValue = None
    fixedLengthPositionTestEither match {
      case Left(_) =>
        fixedLengthPositionTestEither must haveClass[Right[ErrorMessage,FixedLengthPosition]]
      case Right(fixedLengthPosition) =>
        fixedLengthPosition.extractColumnValue(row) mustEqual expectedColumnValue
    }
  }
}
