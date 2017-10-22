package com.scalableQuality.quick.core.fileComponentDescripts

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}

class DelimitedPositionTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with TableDrivenPropertyChecks {

  "DelimitedPosition.apply(MetaData)" should "return Right when position is > 0" in forAll {
    (position: Int) => whenever(position > 0) {
      val columnDescriptionXmlElem = <ColumnDescription position={position.toString} />
      val delimitedPositionTest= DelimitedPosition(columnDescriptionXmlElem.attributes)
      delimitedPositionTest shouldBe a [Right[_,_]]
    }
  }

  it should "return Left when position is < 1" in forAll {
    (position: Int) => whenever(position < 1) {
      val columnDescriptionXmlElem = <ColumnDescription position={position.toString} />
      val delimitedPositionTest= DelimitedPosition(columnDescriptionXmlElem.attributes)
      delimitedPositionTest shouldBe a [Left[_,_]]
    }
  }

  val validPositions = Table(
    "position",
    1,2,3,4,5
  )
  "DelimitedPosition.extractColumnValue" should
    "return Some(Value) when position is 0 < position <= vector.length" in forAll(validPositions) {
    (position: Int) =>
      val row = Vector("H","A","K","I","M")
      val columnDescriptionXmlElem = <ColumnDescription position={position.toString} />
      val delimitedPositionTestEither = DelimitedPosition(columnDescriptionXmlElem.attributes)
      delimitedPositionTestEither match {
        case Left(_) =>
          fail
        case Right(delimitedPositionTest) =>
          delimitedPositionTest.extractColumnValue(row) shouldBe a [Some[_]]
      }
  }

  it should "return none when position > vector.length " in forAll {
    (position: Int) => whenever( position > 5 ) {
      val row = Vector("H","A","K","I","M")
      val columnDescriptionXmlElem = <ColumnDescription position={position.toString} />
      val delimitedPositionTestEither = DelimitedPosition(columnDescriptionXmlElem.attributes)
      delimitedPositionTestEither match {
        case Left(_) =>
          fail
        case Right(delimitedPositionTest) =>
          delimitedPositionTest.extractColumnValue(row) shouldBe None
      }
    }
  }

}
