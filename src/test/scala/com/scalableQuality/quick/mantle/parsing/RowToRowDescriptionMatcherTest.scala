package com.scalableQuality.quick.mantle.parsing

import org.scalatest.{FlatSpec, Matchers}

class RowToRowDescriptionMatcherTest extends FlatSpec with Matchers {

  "RowToRowDescriptionMatcher.apply(Elem)" should "handel both ColumnDescriptions and FixedColumnIdentifier in FixedOrderedRowDescription" in {
    val rowDescriptionElem =
      <FixedOrderedRowDescription label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Right[_,_]]
  }


  it should "handel both upper case and lower case ColumnDescriptions and FixedColumnIdentifier in FixedOrderedRowDescription" in {
    val rowDescriptionElem =
      <FixedOrderedRowDescription label="Track2 data" >
        <COLUMNDESCRIPTION label="seperator" startsAt="1" length="1" />
        <columndescription label="seperator" startsAt="1" length="1" />
        <columnidentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></columnidentifier>
        <COLUMNIDENTIFIER matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></COLUMNIDENTIFIER>
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Right[_,_]]
  }

  it should "return Left[ErrorMessage] when label attribute is absent from FixedOrderedRowDescription" in {
    val rowDescriptionElem =
      <FixedOrderedRowDescription  >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when no Column Identifiers are present in FixedOrderedRowDescription " in {
    val rowDescriptionElem =
      <FixedOrderedRowDescription label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }


  it should "return Left[ErrorMessage] when it encounters an unknown elem" in {
    val rowDescriptionElem =
      <FixedOrderedRowDescription label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
        <MisspelledColumnDiscription />
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it fails to create a FixedColumnDescription" in {
    val rowDescriptionElem =
      <FixedOrderedRowDescription label="Track2 data" >
        <ColumnDescription startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it fails to create a FixedColumnIdentifier" in {
    val rowDescriptionElem = <FixedOrderedRowDescription label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier  label="Visa Card Number" startsAt="2" length="16" ></ColumnIdentifier>
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "handel both ColumnDescriptions and FixedColumnIdentifier in DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription label="Track2 data" literalDelimiter=";" >
        <ColumnDescription label="seperator" position="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" position="2"></ColumnIdentifier>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Right[_,_]]
  }

  it should "handel both upper case and lower case ColumnDescriptions and ColumnIdentifier in DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription label="Track2 data" literalDelimiter=";" >
        <COLUMNDESCRIPTION label="seperator" position="1" />
        <columndescription label="seperator" position="2" />
        <columnidentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" position="3"></columnidentifier>
        <COLUMNIDENTIFIER matchAgainst="^4[0-9]{15}"  label="Visa Card Number" position="1"></COLUMNIDENTIFIER>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Right[_,_]]
  }

  it should "return Left[ErrorMessage] when label attribute is absent from DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription  literalDelimiter=";" >
        <ColumnDescription label="seperator" position="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" position="2"></ColumnIdentifier>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }


  it should "return Left[ErrorMessage] when delimiter attribute is absent from DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription  label="Track2 data" >
        <ColumnDescription label="seperator" position="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" position="2"></ColumnIdentifier>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when no Column Identifiers are present in DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription  label="Track2 data" literalDelimiter=";" >
        <ColumnDescription label="seperator" position="1" />
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it encounters an unknown elem in DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription  label="Track2 data" literalDelimiter=";" >
        <ColumnDescription label="seperator" position="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" position="2"></ColumnIdentifier>
        <MisspelledColumnDiscription />
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it fails to create a ColumnDescription in DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription  label="Track2 data" literalDelimiter=";" >
        <ColumnDescription label="seperator" startsAt="" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" position="2"></ColumnIdentifier>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it fails to create a ColumnIdentifier in DelimitedOrderedRowDescription" in {
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription  label="Track2 data" literalDelimiter=";" >
        <ColumnDescription label="seperator" position="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2"></ColumnIdentifier>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  /**************************************/

  "RowToRowDescriptionMatcher.canIdentify" should "return true when the input string respects the FixedOrderedRowDescription" in {
    val track2dataRow = RawRow(";1234567890123445=99011200XXXX00000000?*",1)
    val rowDescriptionElem =
      <FixedOrderedRowDescription label="Track2 data" >
        <ColumnIdentifier matchAgainst=";" label="Start sentinel" startsAt="1" length="1"/>
        <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" startsAt="2" length="16"/>
        <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" length="1"/>
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither match {
      case Left(errorMessage) =>
        fail(errorMessage.toString)

      case Right(rowIdentifier) =>
        rowIdentifier.canIdentify(track2dataRow) shouldBe true
    }
  }


  it should "return false when the input string does not respect the FixedOrderedRowDescription" in {
    val track2dataRow = RawRow("not track 2",1)
    val rowDescriptionElem =
      <FixedOrderedRowDescription label="Track2 data" >
        <ColumnIdentifier matchAgainst=";" label="Start sentinel" startsAt="1" length="1"/>
        <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" startsAt="2" length="16"/>
        <ColumnIdentifier matchAgainst="=" label="Field separator" startsAt="18" length="1"/>
      </FixedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither match {
      case Left(errorMessage) =>
        fail(errorMessage.toString)

      case Right(rowIdentifier) =>
        rowIdentifier.canIdentify(track2dataRow) shouldBe false
    }
  }

  it should "return true when the input string respects the DelimitedOrderedRowDescription" in {
    val track2dataRow = RawRow(";,1234567890123445,=,99011200XXXX00000000?*",1)
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription label="Track2 data" literalDelimiter="," >
        <ColumnIdentifier matchAgainst=";" label="Start sentinel" position="1"/>
        <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" position="2"/>
        <ColumnIdentifier matchAgainst="=" label="Field separator" position="3"/>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither match {
      case Left(errorMessage) =>
        fail(errorMessage.toString)

      case Right(rowIdentifier) =>
        rowIdentifier.canIdentify(track2dataRow) shouldBe true
    }
  }

  it should "return false when the input string does not respect the DelimitedOrderedRowDescription" in {
    val track2dataRow = RawRow("not track 2",1)
    val rowDescriptionElem =
      <DelimitedOrderedRowDescription label="Track2 data" literalDelimiter="," >
        <ColumnIdentifier matchAgainst=";" label="Start sentinel" position="1"/>
        <ColumnIdentifier matchAgainst="[0-9]{16}" label="Card Number" position="2"/>
        <ColumnIdentifier matchAgainst="=" label="Field separator" position="3"/>
      </DelimitedOrderedRowDescription>
    val rowIdentifierEither = RowToRowDescriptionMatcher(rowDescriptionElem)
    rowIdentifierEither match {
      case Left(errorMessage) =>
        fail(errorMessage.toString)

      case Right(rowIdentifier) =>
        rowIdentifier.canIdentify(track2dataRow) shouldBe false
    }
  }
}
