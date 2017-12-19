package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.fileComponentDescriptions.{
  FixedColumnDescription,
  FixedRowDivider,
  OrderedRowDescription
}
import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalatest.{FlatSpec, Matchers}

class CheckingProcessTest extends FlatSpec with Matchers {
  "CheckingProcess.apply" should "return an empty failed checks if no rows failed the check" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
      checkColumnValueMatches="Row[123]"
      checkColumnValueExists="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringValidation="true"
      checkColumnValueMatches="[a-zA-Z]+"
      checkColumnValueExists="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringValidation="true"
      checkColumnValueMatches="[a-zA-Z]+"
      checkColumnValueExists="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringValidation="true"
      checkColumnValueMatches="[a-zA-Z]+"
      checkColumnValueExists="true"
      />
    val rowNumberColumnDescriptionEither =
      FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)
    (
      rowNumberColumnDescriptionEither,
      firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither
    ) match {
      case (
          Right(rowNumberColumnDescription),
          Right(firstColumnDescription),
          Right(secondColumnDescription),
          Right(thirdColumnDescription)
          ) =>
        val columnDescriptionList = List(rowNumberColumnDescription,
                                         firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "test row")
        val file = List(secondRow, firstRow, thirdRow)
        val expectedResult = (file.reverse, Nil)
        CheckingProcess(orderedRowDescription, file) shouldBe expectedResult
      case _ =>
        fail(
          rowNumberColumnDescriptionEither.toString +
            firstColumnDescriptionEither.toString +
            secondColumnDescriptionEither.toString +
            thirdColumnDescriptionEither.toString
        )
    }
  }

  it should "return rows that passed the test in the left and the ones that failed on the right" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
      checkColumnValueMatches="Row[123]"
      checkColumnValueExists="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringValidation="true"
      />
    val rowNumberColumnDescriptionEither =
      FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)
    (
      rowNumberColumnDescriptionEither,
      firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither
    ) match {
      case (
          Right(rowNumberColumnDescription),
          Right(firstColumnDescription),
          Right(secondColumnDescription),
          Right(thirdColumnDescription)
          ) =>
        val columnDescriptionList = List(rowNumberColumnDescription,
                                         firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "test row")
        val file = List(secondRow, firstRow, thirdRow)
        val expectedResult = (List(thirdRow, firstRow), List(secondRow))
        CheckingProcess(orderedRowDescription, file) shouldBe expectedResult
      case _ =>
        fail(
          rowNumberColumnDescriptionEither.toString +
            firstColumnDescriptionEither.toString +
            secondColumnDescriptionEither.toString +
            thirdColumnDescriptionEither.toString
        )
    }
  }

  it should "return an empty passed checks if all rows have failed the checks" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
      checkColumnValueMatches="[123]"
      checkColumnValueExists="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringValidation="true"
      />
    val rowNumberColumnDescriptionEither =
      FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither =
      FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      FixedColumnDescription(thirdColumnDescriptionElem.attributes)
    (
      rowNumberColumnDescriptionEither,
      firstColumnDescriptionEither,
      secondColumnDescriptionEither,
      thirdColumnDescriptionEither
    ) match {
      case (
          Right(rowNumberColumnDescription),
          Right(firstColumnDescription),
          Right(secondColumnDescription),
          Right(thirdColumnDescription)
          ) =>
        val columnDescriptionList = List(rowNumberColumnDescription,
                                         firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription =
          OrderedRowDescription(fixedRowDivider, "test row")
        val file = List(secondRow, firstRow, thirdRow)
        val expectedResult = (Nil, file.reverse)
        CheckingProcess(orderedRowDescription, file) shouldBe expectedResult
      case _ =>
        fail(
          rowNumberColumnDescriptionEither.toString +
            firstColumnDescriptionEither.toString +
            secondColumnDescriptionEither.toString +
            thirdColumnDescriptionEither.toString
        )
    }
  }
}
