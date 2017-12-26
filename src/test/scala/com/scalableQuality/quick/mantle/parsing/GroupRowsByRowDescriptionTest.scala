package com.scalableQuality.quick.mantle.parsing

import com.scalableQuality.quick.surface.commandLineOptions.QuickState
import org.scalatest.{FlatSpec, Matchers}

class GroupRowsByRowDescriptionTest extends FlatSpec with Matchers {
  "GroupRowsByRowDescription.apply" should "return Right when UnorderedFileDescription is passed as the root elem" in {
    val unorderedFileDescription =
      <UnorderedFileDescription>
      <FixedOrderedRowDescription label="first label" >
        <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
      </FixedOrderedRowDescription>
       <FixedOrderedRowDescription label="first label" >
         <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
       </FixedOrderedRowDescription>
     </UnorderedFileDescription>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Right[_, _]]
  }

  it should
    "return Right() when FilesDescriptionsList contains only one UnorderedFileDescription and no parserId is present" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <UnorderedFileDescription>
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Right[_, _]]
  }

  it should
    "return Right() when FilesDescriptionsList contains multiple UnorderedFileDescription and a file description id is passed" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <UnorderedFileDescription id="firstDesc" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
        <UnorderedFileDescription>
          <FixedOrderedRowDescription label="second label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      Some("firstDesc")
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Right[_, _]]
  }

  it should
    "return Left() when FilesDescriptionsList contains multiple UnorderedFileDescription and a wrong file description id is passed" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <UnorderedFileDescription Id="firstDesc" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
        <UnorderedFileDescription id="secondDesc" >
          <FixedOrderedRowDescription label="second label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      Some("dude")
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should
    "return Left if the root element is neither UnorderedFileDescription nor FilesDescriptionsList" in {
    val unorderedFileDescription =
      <missSpelledFilesDescriptionsList>
        <UnorderedFileDescription>
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </missSpelledFilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should
    "return Left if any of the child elements of FilesDescriptionsList is not UnorderedFileDescription" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <MisspelledUnorderedFileDescription id="firstDesc" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </MisspelledUnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should
    "return Left if any of the the UnorderedFileDescription contains a mistake" in {
    val unorderedFileDescription =
      <UnorderedFileDescription label="Track2 data" >
          <ColumnDescription label="seperator" startsAt="1" length="1" />
          <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
          <MisspelledColumnDiscription />
        </UnorderedFileDescription>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should "return Left if no UnorderedFileDescription is provided inFilesDescriptionsList" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should "return Left if UnorderedFileDescription has no rows" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <UnorderedFileDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should
    "return Left when FilesDescriptionsList contains multiple UnorderedFileDescription and a file no description id is passed" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <UnorderedFileDescription id="firstDesc" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
        <UnorderedFileDescription>
          <FixedOrderedRowDescription label="second label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should
    "return Left when FilesDescriptionsList contains one UnorderedFileDescription and the provided id is wrong" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <UnorderedFileDescription id="firstDesc" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      Some("wrong")
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should
    "return Left if an unknown attribute is present in FilesDescriptionsList" in {
    val unorderedFileDescription =
      <FilesDescriptionsList unknown="attribute" >
        <UnorderedFileDescription>
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      None
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

  it should
    "return Left if an unknown attribute is present in UnorderedFileDescription" in {
    val unorderedFileDescription =
      <FilesDescriptionsList>
        <UnorderedFileDescription id="sup" unknown="attribute" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FilesDescriptionsList>
    val fileDescriptionElem = FileDescriptionElem(
      unorderedFileDescription,
      "",
      Some("sup")
    )
    val RowToRowDescriptionMatcherEither =
      GroupRowsByRowDescription(fileDescriptionElem, None, None, QuickState())
    RowToRowDescriptionMatcherEither shouldBe a[Left[_, _]]
  }

}
