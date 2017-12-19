package com.scalableQuality.quick.core.fileProcessingPhase

import com.scalableQuality.quick.core.fileComponentDescriptions.{
  FixedColumnDescription,
  FixedRowDivider,
  OrderedRowDescription
}
import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalatest.{FlatSpec, Matchers}

class ValidationProcessTest extends FlatSpec with Matchers {
  "ValidationProcess.apply" should "return an empty list if the two files contain the same rows" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = List(secondRow, firstRow, thirdRow)
        val rightFile = List(firstRow, thirdRow, secondRow)
        val expectedResult = (Nil, Nil)
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }

  it should "return left file rows that are absent from the right file" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = List(secondRow, firstRow, thirdRow)
        val rightFile = List(firstRow)
        val expectedResult = (List(thirdRow, secondRow), Nil)
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }
  it should "return right file rows that are absent from the left file" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = List(firstRow)
        val rightFile = List(secondRow, firstRow, thirdRow)
        val expectedResult = (Nil, List(thirdRow, secondRow))
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }
  it should "return all left file rows if the right file is empty" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = List(secondRow, firstRow, thirdRow)
        val rightFile = Nil
        val expectedResult = (List(thirdRow, secondRow, firstRow), Nil)
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }
  it should "return all right file rows if the left file is empty" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = Nil
        val rightFile = List(secondRow, firstRow, thirdRow)
        val expectedResult = (Nil, List(secondRow, firstRow, thirdRow))
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }
  it should "can handle incomplete rows" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = List(secondRow, firstRow, thirdRow)
        val rightFile = List(firstRow, thirdRow, secondRow)
        val expectedResult = (Nil, Nil)
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }

  it should "return left file rows that are different from right file rows and vice versa " in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val differentFirstRow = RawRow("Row1MirstColumnMecondColumnMhirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val differentSecondRow = RawRow("Row2MirstColumnMecondColumnMhirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = List(secondRow, firstRow, thirdRow)
        val rightFile = List(differentSecondRow, thirdRow, differentFirstRow)
        val expectedResult = (List(secondRow, firstRow),
                              List(differentFirstRow, differentSecondRow))
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }

  it should "count the second and the third... occurrence of the same row as invalid " in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn", 1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn", 2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn", 3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringValidation="true"
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
        val leftFile = List(secondRow, firstRow, thirdRow)
        val rightFile = List(firstRow, firstRow, firstRow)
        val expectedResult =
          (List(thirdRow, secondRow), List(firstRow, firstRow))
        ValidationProcess(orderedRowDescription, leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }
}
