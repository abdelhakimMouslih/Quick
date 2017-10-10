package com.scalableQuality.quick.mantle.parsing

import org.scalatest.{FlatSpec, Matchers}

class RowIdentifierTest extends FlatSpec with Matchers {

  "RowIdentifier.apply(Elem)" should "handel both ColumnDescriptions and ColumnIdentifier" in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Right[_,_]]
  }

  it should "handel both upper case and lower case ColumnDescriptions and ColumnIdentifier" in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <COLUMNDESCRIPTION label="seperator" startsAt="1" length="1" />
        <columndescription label="seperator" startsAt="1" length="1" />
        <columnidentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></columnidentifier>
        <COLUMNIDENTIFIER matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></COLUMNIDENTIFIER>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Right[_,_]]
  }

  it should "return Left[ErrorMessage] when label attribute is absent from RowDescription" in {
    val rowDescriptionElem =
      <RowDescrtiption  >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when no Column Identifiers are present " in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }


  it should "return Left[ErrorMessage] when it encounters an unknown elem" in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
        <MisspelledColumnDiscription />
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it fails to create a FixedColumnDescription" in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnDescription startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it fails to create a ColumnIdentifier" in {
    val rowDescriptionElem = <RowDescrtiption label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier  label="Visa Card Number" startsAt="2" length="16" ></ColumnIdentifier>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }


  "RowIdentifier.canIdentify" should "return true when the input string respects the rowDescription" in {
    val track2dataRow = RawRow(";1234567890123445=99011200XXXX00000000?*",1)
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnIdentifier matchAgainst=";" label="Start sentinel" startsAt="1" length="1"/>
        <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" startsAt="2" length="16"/>
        <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" length="1"/>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither match {
      case Left(errorMessage) =>
        fail(errorMessage.toString)

      case Right(rowIdentifier) =>
        rowIdentifier.canIdentify(track2dataRow) shouldBe true
    }
  }


  "RowIdentifier.canIdentify" should "return false when the input string does not respect the rowDescription" in {
    val track2dataRow = RawRow("not track 2",1)
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnIdentifier matchAgainst=";" label="Start sentinel" startsAt="1" length="1"/>
        <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" startsAt="2" length="16"/>
        <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" length="1"/>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither match {
      case Left(errorMessage) =>
        fail(errorMessage.toString)

      case Right(rowIdentifier) =>
        rowIdentifier.canIdentify(track2dataRow) shouldBe false
    }
  }
}
