package com.scalableQuality.quick.core.fileProcessing

import com.scalableQuality.quick.core.fileComponentDescripts.{FixedColumnDescription, OrderedRowDescription}
import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalatest.{FlatSpec, Matchers}

class MatchingProcessTest extends FlatSpec with Matchers {

  "MatchingProcess.apply" should "should match all the rows" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn",1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn",2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn",3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringMatching="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringMatching="true"
      />
    val rowNumberColumnDescriptionEither = FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)
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
        val columnDescriptionList = List(rowNumberColumnDescription,firstColumnDescription,secondColumnDescription,thirdColumnDescription)
        val orderedRowDescription = OrderedRowDescription(columnDescriptionList, "test row")

        val leftFile = List(firstRow, secondRow, thirdRow)
        val rightFile = List(firstRow, secondRow, thirdRow)

        val matchedFirstRow = (Some(firstRow),Some(firstRow))
        val matchedSecondRow = (Some(secondRow),Some(secondRow))
        val matchedThirdRow = (Some(thirdRow),Some(thirdRow))
        val expectedResult = List(matchedThirdRow, matchedSecondRow,matchedFirstRow)

        MatchingProcess(orderedRowDescription,leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }

  it should "return only left file rows is right file is empty " in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn",1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn",2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn",3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringMatching="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringMatching="true"
      />
    val rowNumberColumnDescriptionEither = FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)
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
        val columnDescriptionList = List(rowNumberColumnDescription,firstColumnDescription,secondColumnDescription,thirdColumnDescription)
        val orderedRowDescription = OrderedRowDescription(columnDescriptionList, "test row")

        val leftFile = List(firstRow, secondRow, thirdRow)
        val rightFile = Nil

        val matchedFirstRow = (Some(firstRow),None)
        val matchedSecondRow = (Some(secondRow),None)
        val matchedThirdRow = (Some(thirdRow),None)
        val expectedResult = List(matchedSecondRow, matchedThirdRow,matchedFirstRow)

        MatchingProcess(orderedRowDescription,leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }

  it should "return only right file rows is left file is empty " in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn",1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn",2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn",3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringMatching="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringMatching="true"
      />
    val rowNumberColumnDescriptionEither = FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)
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
        val columnDescriptionList = List(rowNumberColumnDescription,firstColumnDescription,secondColumnDescription,thirdColumnDescription)
        val orderedRowDescription = OrderedRowDescription(columnDescriptionList, "test row")

        val leftFile = Nil
        val rightFile = List(firstRow, secondRow, thirdRow)

        val matchedFirstRow = (None, Some(firstRow))
        val matchedSecondRow = (None, Some(secondRow))
        val matchedThirdRow = (None, Some(thirdRow))
        val expectedResult = List(matchedFirstRow, matchedSecondRow, matchedThirdRow)

        MatchingProcess(orderedRowDescription,leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }

  it should "can handle incomplete rows" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumn",1)
    val secondRow = RawRow("Row2FirstColumnSecondColumn",2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumn",3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringMatching="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringMatching="true"
      />
    val rowNumberColumnDescriptionEither = FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)
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
        val columnDescriptionList = List(rowNumberColumnDescription,firstColumnDescription,secondColumnDescription,thirdColumnDescription)
        val orderedRowDescription = OrderedRowDescription(columnDescriptionList, "test row")

        val leftFile = List(firstRow, secondRow, thirdRow)
        val rightFile = List(firstRow, secondRow, thirdRow)

        val matchedFirstRow = (Some(firstRow),Some(firstRow))
        val matchedSecondRow = (Some(secondRow),Some(secondRow))
        val matchedThirdRow = (Some(thirdRow),Some(thirdRow))
        val expectedResult = List(matchedThirdRow, matchedSecondRow,matchedFirstRow)

        MatchingProcess(orderedRowDescription,leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }


  it should "should match only matched rows" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn",1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn",2)
    val differentSecondRow = RawRow("Row2MirstColumnMecondColumnMhirdColumn",2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn",3)
    val differentThirdRow = RawRow("Row3MirstColumnMecondColumnMhirdColumn",3)

    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringMatching="true"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringMatching="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringMatching="false"
      />

    val rowNumberColumnDescriptionEither = FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)
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
        val columnDescriptionList = List(rowNumberColumnDescription,firstColumnDescription,secondColumnDescription,thirdColumnDescription)
        val orderedRowDescription = OrderedRowDescription(columnDescriptionList, "test row")

        val leftFile = List(firstRow, secondRow, thirdRow)
        val rightFile = List(firstRow, differentSecondRow, differentThirdRow)

        val matchedFirstRow = (Some(firstRow),Some(firstRow))
        val matchedSecondRow = (Some(secondRow),None)
        val matchedDifferentSecondRow = (None,Some(differentSecondRow))
        val matchedThirdRow = (Some(thirdRow),None)
        val matchedDifferentThirdRow = (None,Some(differentThirdRow))
        val expectedResult = List(matchedSecondRow,matchedThirdRow,matchedDifferentThirdRow, matchedDifferentSecondRow,matchedFirstRow )

        MatchingProcess(orderedRowDescription,leftFile, rightFile) shouldBe expectedResult
      case _ => fail("message")
    }
  }

  it should "match no rows from either files if the row description does not have a column description used for matching" in {
    val firstRow = RawRow("Row1FirstColumnSecondColumnThirdColumn",1)
    val secondRow = RawRow("Row2FirstColumnSecondColumnThirdColumn",2)
    val thirdRow = RawRow("Row3FirstColumnSecondColumnThirdColumn",3)
    val rowNumberColumnDescriptionElem = <ColumnDescription
      label="rowNumber"
      startsAt="1"
      endsAt="4"
      useDuringMatching="false"
      />
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="5"
      endsAt="15"
      useDuringMatching="false"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="16"
      endsAt="27"
      useDuringMatching="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="28"
      endsAt="38"
      useDuringMatching="false"
      />
    val rowNumberColumnDescriptionEither = FixedColumnDescription(rowNumberColumnDescriptionElem.attributes)
    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)
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
        val columnDescriptionList = List(rowNumberColumnDescription,firstColumnDescription,secondColumnDescription,thirdColumnDescription)
        val orderedRowDescription = OrderedRowDescription(columnDescriptionList, "test row")

        val leftFile = List(firstRow, secondRow, thirdRow)
        val rightFile = List(firstRow, secondRow, thirdRow)

        val leftFirstMatchedRow = (Some(firstRow), None)
        val leftSecondMatchedRow = (Some(secondRow), None)
        val leftThirdMatchedRow = (Some(thirdRow), None)
        val rightFirstMatchedRow = (None, Some(firstRow))
        val rightSecondMatchedRow = (None, Some(secondRow))
        val rightThirdMatchedRow = (None, Some(thirdRow))
        val expectedResult = List(
          leftFirstMatchedRow,
          leftSecondMatchedRow,
          leftThirdMatchedRow,
          rightFirstMatchedRow,
          rightSecondMatchedRow,
          rightThirdMatchedRow
        )
        MatchingProcess(orderedRowDescription,leftFile, rightFile) shouldBe expectedResult
      case _ => fail
    }
  }
}
