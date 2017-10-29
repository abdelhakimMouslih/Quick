package com.scalableQuality.quick.core.others

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers}

class MatchAgainstTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with TableDrivenPropertyChecks {
  "MatchAgainst.apply(MetaData)" should " return Right(MatchAgainst) with the the specified pattern" in {
    val columnIdentifierElem = <ColumnIdentifier matchAgainst=".*" />
    val matchAgainstEither = MatchAgainst(columnIdentifierElem.attributes)
    matchAgainstEither shouldBe a [Right[_,_]]
  }

  val invalidPatterns = Table(
    "pattern",
    "?",
    "+",
    "*",
    """\""",
    "["
  )
  it should "return Left(_) when the pattern presented is invalid" in forAll(invalidPatterns) {
    (pattern: String) =>
    val columnIdentifierElem = <ColumnIdentifier matchAgainst={pattern} />
    val matchAgainstEither = MatchAgainst(columnIdentifierElem.attributes)
    matchAgainstEither shouldBe a [Left[_,_]]
  }

  it should "return  Left(_) when no matchAgainst Attribute is present" in {
    val columnIdentifierElem = <ColumnIdentifier />
    val matchAgainstEither = MatchAgainst(columnIdentifierElem.attributes)
    matchAgainstEither shouldBe a [Left[_,_]]
  }

  it should "match a string witht the specified pattern" in {
    val pattern = "[0-9]*"
    val inputString = "69696969696969696969696969696"
    val columnIdentifierElem = <ColumnIdentifier matchAgainst={pattern} />
    val matchAgainstEither = MatchAgainst(columnIdentifierElem.attributes)
    matchAgainstEither match {
      case Left(_) =>
        fail()

      case Right(matchAgainst) =>
        matchAgainst(inputString) shouldBe true
    }
  }
}
