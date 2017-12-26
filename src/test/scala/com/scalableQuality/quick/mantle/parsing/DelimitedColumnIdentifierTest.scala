package com.scalableQuality.quick.mantle.parsing

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class DelimitedColumnIdentifierTest
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {
  "DelimitedColumnIdentifier.apply(MetaData)" should
    "return Right(DelimitedColumnIdentifier) when only matchAgainst, label and position are provided" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      position="1"
      />
    val columnIdentifier =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a[Right[_, _]]
  }

  it should
    "return accept DelimitedColumnDescription attributes + matchAgainst" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      position="1"
      useDuringValidation="true"
      useDuringMatching="false"
      useDuringReporting="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="false"
      />
    val columnIdentifier =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a[Right[_, _]]
  }

  it should
    "return Left(ErrorMessage) if matchAgainst attribute is missing" in {
    val columnIdentifierElem = <ColumnIdentifier
      label="columnIdentifier"
      position="1"
      />
    val columnIdentifier =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a[Left[_, _]]
  }

  it should
    "return Left(ErrorMessage) if label attribute is missing" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      position="1"
      />
    val columnIdentifier =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a[Left[_, _]]
  }

  it should
    "return Left(ErrorMessage) if position attribute is missing" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      />
    val columnIdentifier =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a[Left[_, _]]
  }

  it should
    "return Left if a misspelled attribute is present" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      position="1"
      useDuringValidation="true"
      useDuringMatching="false"
      useDuringReporting="true"
      trimComparisonValue="true"
      misspelled="false"
      />
    val columnIdentifier =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a[Left[_, _]]
  }

  it should
    "return Left if an unknown attribute is present" in {
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="matchAgainst"
      label="columnIdentifier"
      position="1"
      useDuringValidation="true"
      useDuringMatching="false"
      useDuringReporting="true"
      trimComparisonValue="true"
      ignoreCase="false"
      unknown="present"
      />
    val columnIdentifier =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifier shouldBe a[Left[_, _]]
  }

  "DelimitedColumnIdentifierTest.apply(RawRow)" should "return true when it identify the targeted substring" in {
    val track2Data = Vector(";", "5301250070000191", "=")
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="^5[0-9]{15}"
      label="MasterCard Card Number"
      position="2"
      />
    val columnIdentifierEither =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifierEither match {
      case Left(_) =>
        fail()

      case Right((_, columnIdentifier)) =>
        columnIdentifier(track2Data) shouldBe true
    }
  }

  it should "return false when it does not identify the targeted substring" in {
    val track2Data = Vector(";", "5301250070000191", "=")
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="^4[0-9]{15}"
      label="Visa Card Number"
      position="2"
      />
    val columnIdentifierEither =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifierEither match {
      case Left(_) =>
        fail()

      case Right((_, columnIdentifier)) =>
        columnIdentifier(track2Data) shouldBe false
    }
  }

  it should "return false when the targeted substring does not exist" in {
    val track2Data = Vector(";", "5301250070000191", "=")
    val columnIdentifierElem = <ColumnIdentifier
      matchAgainst="^4[0-9]{15}"
      label="Visa Card Number"
      position="10"
      />
    val columnIdentifierEither =
      DelimitedColumnIdentifier(columnIdentifierElem.attributes)
    columnIdentifierEither match {
      case Left(_) =>
        fail()

      case Right((_, columnIdentifier)) =>
        columnIdentifier(track2Data) shouldBe false
    }
  }

}
