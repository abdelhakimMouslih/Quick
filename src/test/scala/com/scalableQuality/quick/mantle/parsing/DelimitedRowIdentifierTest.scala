package com.scalableQuality.quick.mantle.parsing

import org.scalatest.{FlatSpec, Matchers}

class DelimitedRowIdentifierTest extends FlatSpec with Matchers {

  "DelimitedRowIdentifier.apply(columnIdentifiers)"should
    "return Left[ErrorMessage] when no columnIdentifiers are supplied" in {
    val delimiterEither = LiteralDelimiter(",")
    delimiterEither match {
      case Right(delimiter) =>
        val delimiterRowIdentifier = DelimitedRowIdentifier(Nil,delimiter)
        delimiterRowIdentifier shouldBe a [Left[_,_]]
      case _ => fail()
    }
  }

  "DelimitedRowIdentifier.canIdentify" should "identify a row when the row is right" in {
    val track2dataRow = RawRow(";,1234567890123445,=,99011200XXXX00000000?*",1)
    val firstColumnIdentifierElem = <ColumnIdentifier matchAgainst=";" label="Start sentinel" position="1" />
    val secondColumnIdentifierElem  = <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" position="2"/>
    val thirdColumnIdentifierElem  = <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" position="3"/>

    val firstColumnIdentifierEither = DelimitedColumnIdentifier(firstColumnIdentifierElem.attributes)
    val secondColumnIdentifierEither  = DelimitedColumnIdentifier(secondColumnIdentifierElem.attributes)
    val thirdColumnIdentifierEither  = DelimitedColumnIdentifier(thirdColumnIdentifierElem.attributes)

    val delimiterEither = LiteralDelimiter(",")
    (firstColumnIdentifierEither, secondColumnIdentifierEither, thirdColumnIdentifierEither,delimiterEither) match {
      case (Right((_,firstColumnIdentifier)),Right((_,secondColumnIdentifier)),Right((_,thirdColumnIdentifier)),Right(delimiter)) =>
        val identifiersList = List(firstColumnIdentifier,secondColumnIdentifier,thirdColumnIdentifier)
        val rowIdentifierEither = DelimitedRowIdentifier(identifiersList, delimiter)
        rowIdentifierEither match {
          case Right(rowIdentifier) =>
            rowIdentifier.canIdentify(track2dataRow) shouldBe true
          case _ => fail()
        }
      case _ => fail
    }
  }

  it should "not identify a row when the row is not right" in {
    val track2dataRow = RawRow("not track 2",1)
    val firstColumnIdentifierElem = <ColumnIdentifier matchAgainst=";" label="Start sentinel" position="1" />
    val secondColumnIdentifierElem  = <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" position="2"/>
    val thirdColumnIdentifierElem  = <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" position="3"/>

    val firstColumnIdentifierEither = DelimitedColumnIdentifier(firstColumnIdentifierElem.attributes)
    val secondColumnIdentifierEither  = DelimitedColumnIdentifier(secondColumnIdentifierElem.attributes)
    val thirdColumnIdentifierEither  = DelimitedColumnIdentifier(thirdColumnIdentifierElem.attributes)

    val delimiterEither = LiteralDelimiter(",")
    (firstColumnIdentifierEither, secondColumnIdentifierEither, thirdColumnIdentifierEither,delimiterEither) match {
      case (Right((_,firstColumnIdentifier)),Right((_,secondColumnIdentifier)),Right((_,thirdColumnIdentifier)),Right(delimiter)) =>
        val identifiersList = List(firstColumnIdentifier,secondColumnIdentifier,thirdColumnIdentifier)
        val rowIdentifierEither = DelimitedRowIdentifier(identifiersList, delimiter)
        rowIdentifierEither match {
          case Right(rowIdentifier) =>
            rowIdentifier.canIdentify(track2dataRow) shouldBe false
          case _ => fail()
        }
      case _ => fail
    }
  }
}
