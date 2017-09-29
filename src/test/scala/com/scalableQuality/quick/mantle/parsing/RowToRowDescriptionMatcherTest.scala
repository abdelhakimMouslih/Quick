package com.scalableQuality.quick.mantle.parsing

import org.scalatest.{FlatSpec, Matchers}

class RowToRowDescriptionMatcherTest extends FlatSpec with Matchers {
  "RowToRowDescriptionMatcher.apply" should "return Right when UnorderedFileDescription is passed as the root elem" in {
   val unorderedFileDescription =
     <UnorderedFileDescription>
      <OrderedRowDescription label="first label" >
        <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
      </OrderedRowDescription>
     </UnorderedFileDescription>
    val RowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Right[_,_]]
  }

  it should
    "return Right() when FileDescriptionsList contains only one UnorderedFileDescription and no parserId is present" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <UnorderedFileDescription>
          <OrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </OrderedRowDescription>
        </UnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Right[_,_]]
  }

  it should
  "return Right() when FileDescriptionsList contains multiple UnorderedFileDescription and a file description id is passed" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <UnorderedFileDescription id="firstDesc" >
          <OrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </OrderedRowDescription>
        </UnorderedFileDescription>
        <UnorderedFileDescription>
          <OrderedRowDescription label="second label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </OrderedRowDescription>
        </UnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(unorderedFileDescription, Some("firstDesc"), None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Right[_,_]]
  }

  it should
    "return Left() when FileDescriptionsList contains multiple UnorderedFileDescription and a wrong file description id is passed" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <UnorderedFileDescription id="firstDesc" >
          <OrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </OrderedRowDescription>
        </UnorderedFileDescription>
        <UnorderedFileDescription>
          <OrderedRowDescription label="second label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </OrderedRowDescription>
        </UnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(unorderedFileDescription, Some("dude"), None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
  "return Left if the root element is neither UnorderedFileDescription nor FileDescriptionsList" in {
    val unorderedFileDescription =
      <missSpelledFileDescriptionsList>
        <UnorderedFileDescription>
          <OrderedRowDescription label="first label" >
            <ColumnIdentifier matchAgainst="99" label="labeling" startsAt="300" length="48"/>
          </OrderedRowDescription>
        </UnorderedFileDescription>
      </missSpelledFileDescriptionsList>
    val RowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
    "return Left if any of the child elements of FileDescriptionsList is not UnorderedFileDescription" in {
    val unorderedFileDescription =
      <FileDescriptionsList>
        <missSpelledUnorderedFileDescription id="firstDesc" >
          <OrderedRowDescription label="first label" >
            <ColumnIdentifier label="labeling" startsAt="300" length="48"/>
          </OrderedRowDescription>
        </missSpelledUnorderedFileDescription>
      </FileDescriptionsList>
    val RowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]
  }

  it should
  "return Left if any of the the UnorderedFileDescription contains a mistake" in {
    val unorderedFileDescription =
      <UnorderedFileDescription>
        <UnorderedFileDescription label="Track2 data" >
          <ColumnDescription label="seperator" startsAt="1" length="1" />
          <ColumnIdentifier matchAgainst="^4[0-9]{15}"  label="Visa Card Number" startsAt="2" length="16"></ColumnIdentifier>
          <MisspelledColumnDiscription />
        </UnorderedFileDescription>
      </UnorderedFileDescription>
    val RowToRowDescriptionMatcherEither = RowToRowDescriptionMatcher(unorderedFileDescription, None, None, None)
    RowToRowDescriptionMatcherEither shouldBe a [Left[_,_]]

  }

}
