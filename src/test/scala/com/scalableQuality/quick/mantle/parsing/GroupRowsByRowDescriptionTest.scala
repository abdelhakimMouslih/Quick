package com.scalableQuality.quick.mantle.parsing

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
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Right[_,_]]
  }

  it should
    "return Right() when FileDescriptionsList contains only one UnorderedFileDescription and no parserId is present" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <UnorderedFileDescription>
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Right[_,_]]
  }

  it should
  "return Right() when FileDescriptionsList contains multiple UnorderedFileDescription and a file description id is passed" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
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
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, Some("firstDesc"), None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Right[_,_]]
  }

  it should
    "return Left() when FileDescriptionsList contains multiple UnorderedFileDescription and a wrong file description id is passed" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
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
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, Some("dude"), None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
  "return Left if the root element is neither UnorderedFileDescription nor FileDescriptionsList" in {
    val unorderedFileDescription =
      <missSpelledFileDescriptionsList>
        <UnorderedFileDescription>
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </missSpelledFileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
    "return Left if any of the child elements of FileDescriptionsList is not UnorderedFileDescription" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <MisspelledUnorderedFileDescription id="firstDesc" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </MisspelledUnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
  "return Left if any of the the UnorderedFileDescription contains a mistake" in {
    val unorderedFileDescription =
        <UnorderedFileDescription label="Track2 data" >
          <ColumnDescription label="seperator" startsAt="1" length="1" />
          <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
          <MisspelledColumnDiscription />
        </UnorderedFileDescription>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should "return Left if no UnorderedFileDescription is provided inFileDescriptionsList" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should "return Left if UnorderedFileDescription has no rows" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <UnorderedFileDescription>
        </UnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
    "return Left when FileDescriptionsList contains multiple UnorderedFileDescription and a file no description id is passed" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
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
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
    "return Left when FileDescriptionsList contains one UnorderedFileDescription and the provided id is wrong" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <UnorderedFileDescription id="firstDesc" >
          <FixedOrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </FixedOrderedRowDescription>
        </UnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = GroupRowsByRowDescription(unorderedFileDescription, Some("wrong"), None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

}
