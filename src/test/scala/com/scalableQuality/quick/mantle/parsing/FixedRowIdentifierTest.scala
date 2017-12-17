package com.scalableQuality.quick.mantle.parsing

import org.scalatest.{FlatSpec, Matchers}

class FixedRowIdentifierTest extends FlatSpec with Matchers {

  "FixedRowIdentifier.apply(columnIdentifiers)" should
    "return Left[ErrorMessage] when no columnIdentifiers are supplied" in {
    val fixedRowIdentifier = FixedRowIdentifier(Nil)
    fixedRowIdentifier shouldBe a[Left[_, _]]
  }

  "FixedRowIdentifier.canIdentify" should "identify a row when the row is right" in {
    val track2dataRow = RawRow(";1234567890123445=99011200XXXX00000000?*", 1)
    val firstColumnIdentifierElem =
      <ColumnIdentifier matchAgainst=";" label="Start sentinel" startsAt="1" length="1"/>
    val secondColumnIdentifierElem =
      <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" startsAt="2" length="16"/>
    val thirdColumnIdentifierElem =
      <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" length="1"/>

    val firstColumnIdentifierEither =
      FixedColumnIdentifier(firstColumnIdentifierElem.attributes)
    val secondColumnIdentifierEither =
      FixedColumnIdentifier(secondColumnIdentifierElem.attributes)
    val thirdColumnIdentifierEither =
      FixedColumnIdentifier(thirdColumnIdentifierElem.attributes)

    (firstColumnIdentifierEither,
     secondColumnIdentifierEither,
     thirdColumnIdentifierEither) match {
      case (Right((_, firstColumnIdentifier)),
            Right((_, secondColumnIdentifier)),
            Right((_, thirdColumnIdentifier))) =>
        val identifiersList: List[FixedColumnIdentifier] =
          List(firstColumnIdentifier,
               secondColumnIdentifier,
               thirdColumnIdentifier)
        val rowIdentifierEither = FixedRowIdentifier(identifiersList)
        rowIdentifierEither match {
          case Right(rowIdentifier: FixedRowIdentifier) =>
            rowIdentifier.canIdentify(track2dataRow) shouldBe true
          case _ => fail
        }
      case _ => fail
    }
  }

  it should "not identify a row when the row is not right" in {
    val track2dataRow = RawRow("not track 2", 1)
    val firstColumnIdentifierElem =
      <ColumnIdentifier matchAgainst=";" label="Start sentinel" startsAt="1" length="1"/>
    val secondColumnIdentifierElem =
      <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" startsAt="2" length="16"/>
    val thirdColumnIdentifierElem =
      <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" length="1"/>

    val firstColumnIdentifierEither =
      FixedColumnIdentifier(firstColumnIdentifierElem.attributes)
    val secondColumnIdentifierEither =
      FixedColumnIdentifier(secondColumnIdentifierElem.attributes)
    val thirdColumnIdentifierEither =
      FixedColumnIdentifier(thirdColumnIdentifierElem.attributes)

    (firstColumnIdentifierEither,
     secondColumnIdentifierEither,
     thirdColumnIdentifierEither) match {
      case (Right((_, firstColumnIdentifier)),
            Right((_, secondColumnIdentifier)),
            Right((_, thirdColumnIdentifier))) =>
        val identifiersList: List[FixedColumnIdentifier] =
          List(firstColumnIdentifier,
               secondColumnIdentifier,
               thirdColumnIdentifier)
        val rowIdentifierEither = FixedRowIdentifier(identifiersList)
        rowIdentifierEither match {
          case Right(rowIdentifier: FixedRowIdentifier) =>
            rowIdentifier.canIdentify(track2dataRow) shouldBe false
          case _ => fail
        }
      case _ => fail
    }
  }

  it should "not identify an empty row " in {
    val track2dataRow = RawRow("", 1)
    val firstColumnIdentifierElem =
      <ColumnIdentifier matchAgainst=";" label="Start sentinel" startsAt="1" length="1"/>
    val secondColumnIdentifierElem =
      <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" startsAt="2" length="16"/>
    val thirdColumnIdentifierElem =
      <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" length="1"/>

    val firstColumnIdentifierEither =
      FixedColumnIdentifier(firstColumnIdentifierElem.attributes)
    val secondColumnIdentifierEither =
      FixedColumnIdentifier(secondColumnIdentifierElem.attributes)
    val thirdColumnIdentifierEither =
      FixedColumnIdentifier(thirdColumnIdentifierElem.attributes)

    (firstColumnIdentifierEither,
     secondColumnIdentifierEither,
     thirdColumnIdentifierEither) match {
      case (Right((_, firstColumnIdentifier)),
            Right((_, secondColumnIdentifier)),
            Right((_, thirdColumnIdentifier))) =>
        val identifiersList: List[FixedColumnIdentifier] =
          List(firstColumnIdentifier,
               secondColumnIdentifier,
               thirdColumnIdentifier)
        val rowIdentifierEither = FixedRowIdentifier(identifiersList)
        rowIdentifierEither match {
          case Right(rowIdentifier: FixedRowIdentifier) =>
            rowIdentifier.canIdentify(track2dataRow) shouldBe false
          case _ => fail
        }
      case _ => fail
    }
  }
}
