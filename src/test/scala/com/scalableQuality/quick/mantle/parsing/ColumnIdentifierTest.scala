package com.scalableQuality.quick.mantle.parsing

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ColumnIdentifierTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "ColumnIdentifier.apply(MetaData)" should
  "return Right(ColumnIdentifier) even if only matchAgainst, label, startsAt and endsAt are present" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      startsAt="1"
      endsAt="3"
      />
    val columnIdentifier = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a [Right[_,_]]
  }

  it should
    "return Right(ColumnIdentifier) even if only matchAgainst, label, startsAt and length are present" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      startsAt="1"
      length="3"
      />
    val columnIdentifier = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a [Right[_,_]]
  }

  it should
  "return Left(ErrorMessage) if matchAgainst attribute is missing" in {
    val columnIdentifierElem = <ColumnIdentifier
      label="columnIdentifier"
      startsAt="1"
      length="3"
      />
    val columnIdentifier = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a [Left[_,_]]
  }

  it should
  "return Left(ErrorMessage) if label attribute is missing" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      startsAt="1"
      length="3"
      />
    val columnIdentifier = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a [Left[_,_]]
  }

  it should
    "return Left(ErrorMessage) if startsAt attribute is missing" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      length="3"
      />
    val columnIdentifier = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a [Left[_,_]]
  }

  it should
    "return Left(ErrorMessage) if endsAt and length attributes are missing" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      startsAt="1"
      />
    val columnIdentifier = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a [Left[_,_]]
  }

  "ColumnIdentifier.apply(RawRow)" should "return true when it identify the targeted substring" in {
    val track2Data = RawRow(";5301250070000191=08051010912345678901?3",1)
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="^5[0-9]{15}"
      label="MasterCard Card Number"
      startsAt="2"
      length="16"
      />
    val columnIdentifierEither = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifierEither match {
      case Left(_) =>
        fail()

      case Right((_,columnIdentifier)) =>
        columnIdentifier(track2Data) shouldBe true
    }
  }

  it should "return false when it does not identify the targeted substring" in {
    val track2Data = RawRow(";5301250070000191=08051010912345678901?3",1)
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="^4[0-9]{15}"
      label="Visa Card Number"
      startsAt="2"
      length="16"
      />
    val columnIdentifierEither = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifierEither match {
      case Left(_) =>
        fail()

      case Right((_,columnIdentifier)) =>
        columnIdentifier(track2Data) shouldBe false
    }
  }

  it should "return false when the targeted substring does not exist" in {
    val track2Data = RawRow(";5301250070000191=08051010912345678901?3",1)
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="^4[0-9]{15}"
      label="Visa Card Number"
      startsAt="2"
      length="100"
      />
    val columnIdentifierEither = ColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifierEither match {
      case Left(_) =>
        fail()

      case Right((_,columnIdentifier)) =>
        columnIdentifier(track2Data) shouldBe false
    }
  }

  val invalidPatterns = List(
    "?",
    "+",
    "*",
    """\"""
  )
  it should "return true if the whole string got identifier" in forAll{
    (inputString: String) => whenever(inputString.length > 0 && ! invalidPatterns.contains(inputString)){
      val inputStringRow = RawRow(inputString, 1)
      val columnIdentifierElem = <ColumnIdentifier
        matchAgainst={inputString}
        label="input string"
        startsAt="1"
        length={inputString.length.toString}
        />
      val columnIdentifierEither = ColumnIdentifier(columnIdentifierElem.attributes)
      columnIdentifierEither match {
        case Left(_) =>
          fail()

        case Right((_,columnIdentifier)) =>
          columnIdentifier(inputStringRow) shouldBe true
      }
    }
  }

}
