package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.Reporting.{
  InvalidColumns,
  IrrelevantColumns,
  ReportingColumns,
  ValidColumns
}
import com.scalableQuality.quick.core.phases.{
  MatchingStage,
  ReportingStage,
  ValidationStage
}
import com.scalableQuality.quick.mantle.parsing.{LiteralDelimiter, RawRow}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class DelimitedRowDividerTest
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {
  "DelimitedRowDivider.isMatchable" should
    "return true if at least one could should be used in matching and false otherwise" in forAll {
    (firstColumnMatching: Boolean,
     secondColumnMatching: Boolean,
     thirdColumnMatching: Boolean) =>
      val firstColumnDescriptionElem = <ColumnDescription
        label="firstColumn"
        position="1"
        useDuringMatching={firstColumnMatching.toString}/>
      val secondColumnDescriptionElem = <ColumnDescription
        label="secondColumn"
        position="2"
        useDuringMatching={secondColumnMatching.toString}/>

      val thirdColumnDescriptionElem = <ColumnDescription
        label="thirdColumn"
        position="3"
        useDuringMatching={thirdColumnMatching.toString}/>
      val firstColumnDescriptionEither =
        DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
      val secondColumnDescriptionEither =
        DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
      val thirdColumnDescriptionEither =
        DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

      val delimiterEither = LiteralDelimiter(";")
      (firstColumnDescriptionEither,
       secondColumnDescriptionEither,
       thirdColumnDescriptionEither,
       delimiterEither) match {
        case (Right(firstColumnDescription),
              Right(secondColumnDescription),
              Right(thirdColumnDescription),
              Right(delimiter)) =>
          val columnDescriptionList = List(firstColumnDescription,
                                           secondColumnDescription,
                                           thirdColumnDescription)
          val delimitedRowDivider =
            DelimitedRowDivider(columnDescriptionList, delimiter)
          delimitedRowDivider.isMatchable shouldBe (firstColumnMatching || secondColumnMatching || thirdColumnMatching)
        case _ => fail
      }
  }

  "DelimitedRowDivider.keepOnlyColumnsDescriptionsUsedIn" should
    "return an DelimitedRowDivider containing all columns used during validation " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      position="2"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      position="2"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      position="2"
      />
    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("|")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)

        val expectedColumnDescriptionList =
          List(firstColumnDescription, secondColumnDescription)
        val expectedDelimitedRowDivider =
          DelimitedRowDivider(expectedColumnDescriptionList, delimiter)
        delimitedRowDivider.keepOnlyColumnsDescriptionsUsedIn(ValidationStage) shouldBe expectedDelimitedRowDivider
      case _ => fail
    }
  }

  it should
    "return an DelimitedRowDivider containing all columns used during Matching " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      position="2"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      position="2"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      position="2"
      useDuringMatching="true"
      />
    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("|")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)

        val expectedColumnDescriptionList =
          List(firstColumnDescription, thirdColumnDescription)
        val expectedDelimitedRowDivider =
          DelimitedRowDivider(expectedColumnDescriptionList, delimiter)

        delimitedRowDivider.keepOnlyColumnsDescriptionsUsedIn(MatchingStage) shouldBe expectedDelimitedRowDivider
      case _ => fail
    }
  }

  it should
    "return an DelimitedRowDivider containing all columns used during Reporting " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      position="2"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      position="2"
      useDuringReporting="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      position="2"
      useDuringReporting="true"
      />
    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("|")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)

        val expectedColumnDescriptionList =
          List(secondColumnDescription, thirdColumnDescription)
        val expectedDelimitedRowDivider =
          DelimitedRowDivider(expectedColumnDescriptionList, delimiter)

        delimitedRowDivider.keepOnlyColumnsDescriptionsUsedIn(ReportingStage) shouldBe expectedDelimitedRowDivider
      case _ => fail
    }
  }

  it should
    "return an DelimitedRowDivider containing all columns used during from different stages " in {
    val firstColumnDescriptionElem = <ColumnDescription
      label="firstColumn"
      position="2"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="secondColumn"
      position="2"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="thirdColumn"
      position="2"
      useDuringReporting="true"
      />
    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("|")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)

        val expectedColumnDescriptionList = List(firstColumnDescription,
                                                 secondColumnDescription,
                                                 thirdColumnDescription)
        val expectedDelimitedRowDivider =
          DelimitedRowDivider(expectedColumnDescriptionList, delimiter)

        delimitedRowDivider.keepOnlyColumnsDescriptionsUsedIn(
          ValidationStage,
          MatchingStage,
          ReportingStage) shouldBe expectedDelimitedRowDivider
      case _ => fail
    }
  }

  "DelimitedRowDivider.columnsComparisonValuesFor" should "extract column values of all 3 validation columns" in {
    val rawRow = RawRow("FirstColumn;SecondColumn;ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter(";")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)
        val expectedColumnValues =
          List(Some("FirstColumn"), Some("SecondColumn"), Some("ThirdColumn"))
        delimitedRowDivider.columnsComparisonValuesFor(ValidationStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "extract column values of only validation columns" in {
    val rawRow = RawRow("FirstColumn|SecondColumn|ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("|")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)

        val expectedColumnValues =
          List(Some("FirstColumn"), Some("ThirdColumn"))
        delimitedRowDivider.columnsComparisonValuesFor(ValidationStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "the extracted column values should include None for every validation column that does not exist" in {
    val rawRow = RawRow("FirstColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("|")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)
        val expectedColumnValues = List(Some("FirstColumn"), None, None)

        delimitedRowDivider.columnsComparisonValuesFor(ValidationStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "extract column values of all 3 matching columns" in {
    val rawRow = RawRow("FirstColumn|SecondColumn|ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("|")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)
        val expectedColumnValues =
          List(Some("FirstColumn"), Some("SecondColumn"), Some("ThirdColumn"))
        delimitedRowDivider.columnsComparisonValuesFor(MatchingStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "extract column values of only matching columns" in {
    val rawRow = RawRow("FirstColumn*SecondColumn*ThirdColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("*")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)

        val expectedColumnValues =
          List(Some("FirstColumn"), Some("ThirdColumn"))
        delimitedRowDivider.columnsComparisonValuesFor(MatchingStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  it should "the extracted column values should include None for every matching column that does not exist" in {
    val rawRow = RawRow("FirstColumn", 1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      position="1"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      position="2"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      position="3"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither =
      DelimitedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither =
      DelimitedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither =
      DelimitedColumnDescription(thirdColumnDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter("*")
    (firstColumnDescriptionEither,
     secondColumnDescriptionEither,
     thirdColumnDescriptionEither,
     delimiterEither) match {
      case (Right(firstColumnDescription),
            Right(secondColumnDescription),
            Right(thirdColumnDescription),
            Right(delimiter)) =>
        val columnDescriptionList = List(firstColumnDescription,
                                         secondColumnDescription,
                                         thirdColumnDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)
        val expectedColumnValues = List(Some("FirstColumn"), None, None)

        delimitedRowDivider.columnsComparisonValuesFor(MatchingStage, rawRow) shouldBe expectedColumnValues
      case _ => fail
    }
  }

  "DelimitedRowDivider.compare" should "return ValidColumns, IrrelevantColumns, ReportingColumns and InvalidColumns" in {
    val leftRawRow = Some(RawRow("One;Two;Three;Four", 1))
    val rightRawRow = Some(RawRow("One;Bwo;RRree;Sour", 2))
    val validColumnsDescriptionElem = <ColumnDescription
      label="valid column"
      position="1"
      useDuringValidation="true"
      useDuringReporting="false"
      />
    val irrelevantColumnsDescriptionElem = <ColumnDescription
      label="irrelevant column"
      position="2"
      useDuringValidation="false"
      useDuringReporting="false"
      />
    val reportingColumnsDescriptionElem = <ColumnDescription
      label="reporting column"
      position="3"
      useDuringValidation="false"
      useDuringReporting="true"
      />
    val invalidColumnsDescriptionElem = <ColumnDescription
      label="invalid column"
      position="4"
      useDuringValidation="true"
      useDuringReporting="false"
      />
    val validColumnsDescriptionEither =
      DelimitedColumnDescription(validColumnsDescriptionElem.attributes)
    val irrelevantColumnsDescriptionEither =
      DelimitedColumnDescription(irrelevantColumnsDescriptionElem.attributes)
    val reportingColumnsDescriptionEither =
      DelimitedColumnDescription(reportingColumnsDescriptionElem.attributes)
    val invalidColumnsDescriptionEither =
      DelimitedColumnDescription(invalidColumnsDescriptionElem.attributes)

    val delimiterEither = LiteralDelimiter(";")
    (
      validColumnsDescriptionEither,
      irrelevantColumnsDescriptionEither,
      reportingColumnsDescriptionEither,
      invalidColumnsDescriptionEither,
      delimiterEither
    ) match {
      case (
          Right(validColumnsDescription),
          Right(irrelevantColumnsDescription),
          Right(reportingColumnsDescription),
          Right(invalidColumnsDescription),
          Right(delimiter)
          ) =>
        val columnDescriptionList = List(validColumnsDescription,
                                         irrelevantColumnsDescription,
                                         reportingColumnsDescription,
                                         invalidColumnsDescription)
        val delimitedRowDivider =
          DelimitedRowDivider(columnDescriptionList, delimiter)

        val expectedReportingColumns = ReportingColumns(
          Some("Three"),
          Some("RRree"),
          "   3",
          "reporting column"
        )

        val expectedInvalidColumns = InvalidColumns(
          Some("Four"),
          Some("Sour"),
          "   4",
          "invalid column"
        )

        val expectedComparison = List(ValidColumns,
                                      IrrelevantColumns,
                                      expectedReportingColumns,
                                      expectedInvalidColumns)

        delimitedRowDivider.compare(leftRawRow, rightRawRow) shouldBe expectedComparison
      case _ =>
        fail
    }
  }

}
