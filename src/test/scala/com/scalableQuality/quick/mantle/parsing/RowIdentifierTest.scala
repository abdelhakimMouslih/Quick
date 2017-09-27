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

  "RowIdentifier.apply(Elem)" should "handel both upper case and lower case ColumnDescriptions and ColumnIdentifier" in {
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

  it should "return Left[ErrorMessage] when no columnDescription and Column Identifiers are present " in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
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

  it should "return Left[ErrorMessage] when it fails to create a ColumnDescription" in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnDescription startsAt="1" length="1" />
        <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

  it should "return Left[ErrorMessage] when it fails to create a ColumnIdentifier" in {
    val rowDescriptionElem =
      <RowDescrtiption label="Track2 data" >
        <ColumnDescription label="seperator" startsAt="1" length="1" />
        <ColumnIdentifier  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
      </RowDescrtiption>
    val rowIdentifierEither = RowIdentifier(rowDescriptionElem)
    rowIdentifierEither shouldBe a [Left[_,_]]
  }

}
