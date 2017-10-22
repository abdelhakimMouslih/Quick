package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.core.Reporting.{InvalidColumns, IrrelevantColumns, ReportingColumns, ValidColumns}
import com.scalableQuality.quick.core.others.{MatchingStage, ReportingStage, ValidationStage}
import com.scalableQuality.quick.mantle.parsing.RawRow
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class OrderedRowDescriptionTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {


  "OrderedRowDescription.validationSignatureOf" should "return the signature of all 3 validation columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn",1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither, secondColumnDescriptionEither, thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),Right(secondColumnDescription),Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription, secondColumnDescription, thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription = OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(Some(List(-116, 54, 9, -51, -17, 108, -101, -69, -70, 113, 67, -10, -19, 108, 97, -93, -36, 65, 99, 40)))

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "return the signature of only validation columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn",1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither, secondColumnDescriptionEither, thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),Right(secondColumnDescription),Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription, secondColumnDescription, thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription = OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(Some(List(97, -35, -42, -100, 114, 86, 61, 15, 103, -92, -59, 0, -115, 21, -4, -44, 39, -93, 90, -117)))

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "the signature should include None for every validation column that does not exist" in {
    val rawRow = RawRow("FirstColumn",1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringValidation="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringValidation="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringValidation="true"
      />

    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither, secondColumnDescriptionEither, thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),Right(secondColumnDescription),Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription, secondColumnDescription, thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription = OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature =
          List(Some(List(-34, 6, -107, 11, -85, -124, -114, -75, 104, -5, -21, -57, 26, 27, 9, 38, -21, 44, -79, 94)),None,None)

        orderedRowDescription.validationSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }


  "OrderedRowDescription.matchingSignatureOf" should "return the signature of all 3 matching columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn",1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither, secondColumnDescriptionEither, thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),Right(secondColumnDescription),Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription, secondColumnDescription, thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription = OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(Some(List(-116, 54, 9, -51, -17, 108, -101, -69, -70, 113, 67, -10, -19, 108, 97, -93, -36, 65, 99, 40)))

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }

  it should "return the signature of only matching columns" in {
    val rawRow = RawRow("FirstColumnSecondColumnThirdColumn",1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="false"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither, secondColumnDescriptionEither, thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),Right(secondColumnDescription),Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription, secondColumnDescription, thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription = OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature = List(Some(List(97, -35, -42, -100, 114, 86, 61, 15, 103, -92, -59, 0, -115, 21, -4, -44, 39, -93, 90, -117)))

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }
  it should "the signature should include None for every matching column that does not exist" in {
    val rawRow = RawRow("FirstColumn",1)
    val firstColumnDescriptionElem = <ColumnDescription
      label="FirstColumn"
      startsAt="1"
      endsAt="11"
      useDuringMatching="true"
      />
    val secondColumnDescriptionElem = <ColumnDescription
      label="SecondColumn"
      startsAt="12"
      endsAt="23"
      useDuringMatching="true"
      />

    val thirdColumnDescriptionElem = <ColumnDescription
      label="ThirdColumn"
      startsAt="24"
      endsAt="34"
      useDuringMatching="true"
      />

    val firstColumnDescriptionEither = FixedColumnDescription(firstColumnDescriptionElem.attributes)
    val secondColumnDescriptionEither = FixedColumnDescription(secondColumnDescriptionElem.attributes)
    val thirdColumnDescriptionEither = FixedColumnDescription(thirdColumnDescriptionElem.attributes)

    (firstColumnDescriptionEither, secondColumnDescriptionEither, thirdColumnDescriptionEither) match {
      case (Right(firstColumnDescription),Right(secondColumnDescription),Right(thirdColumnDescription)) =>
        val columnDescriptionList = List(firstColumnDescription, secondColumnDescription, thirdColumnDescription)
        val fixedRowDivider = FixedRowDivider(columnDescriptionList)
        val orderedRowDescription = OrderedRowDescription(fixedRowDivider, "label")

        val expectedSignature =
          List(Some(List(-34, 6, -107, 11, -85, -124, -114, -75, 104, -5, -21, -57, 26, 27, 9, 38, -21, 44, -79, 94)),None,None)

        orderedRowDescription.matchingSignatureOf(rawRow) shouldBe expectedSignature
      case _ => fail
    }
  }


}
