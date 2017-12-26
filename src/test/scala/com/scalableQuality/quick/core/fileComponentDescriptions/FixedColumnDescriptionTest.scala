package com.scalableQuality.quick.core.fileComponentDescriptions

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FixedColumnDescriptionTest
    extends FlatSpec
    with Matchers
    with TableDrivenPropertyChecks {
  "FixedColumnDescription.apply(MetaData)" should
    "return Right[ErrorMessage, FixedColumnDescription] even if only the label, the startsAt and endsAt  attributes are present" in {
    val columnDescriptionElem = <ColumnDescription
       label="this is a column in russian accent"
       startsAt="1"
       endsAt="3"
      />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Right[_, _]]
  }

  it should
    "return Right[ErrorMessage, FixedColumnDescription] even if only the label, the startsAt and length  attributes are present" in {
    val columnDescriptionElem = <ColumnDescription
      label="this is a column in russian accent"
      startsAt="1"
      length="3"
      />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Right[_, _]]
  }

  it should
    "accepts all the following attributes, label, startsAt, endsAt, useDuringValidation, useDuringMatching, useDuringReporting, trimComparisonValue, ignoreComparisonValueCase" in {
    val columnDescriptionElem = <ColumnDescription
    label="this is a column in russian accent"
    startsAt="1"
    endsAt="3"
    length="3"
    useDuringValidation="true"
    useDuringMatching="false"
    useDuringReporting="true"
    trimComparisonValue="true"
    ignoreComparisonValueCase="false" />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Right[_, _]]
  }

  it should "return Left a misspelled attribute is present" in {
    val columnDescriptionElem = <FixedColumnDescription
      label="this is a column in russian accent"
      startsAt="1"
      endsAt="3"
      length="3"
      useDuringValidation="true"
      useDuringMatching="false"
      useDuringReporting="true"
      trimComparisonValue="true"
      misspelledAttribute="false"
      />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Left[_, _]]
  }

  it should "return Left a unknown attribute is present" in {
    val columnDescriptionElem = <FixedColumnDescription
      label="this is a column in russian accent"
      startsAt="1"
      endsAt="3"
      length="3"
      useDuringValidation="true"
      useDuringMatching="false"
      useDuringReporting="true"
      trimComparisonValue="true"
      ignoreComparisonValueCase="false"
      unknown="present" />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Left[_, _]]
  }

  val invalidAttributeValue = Table(
    ("startsAt",
     "endsAt",
     "length",
     "useDuringValidation",
     "useDuringMatching",
     "useDuringReporting",
     "trimComparisonValue",
     "ignoreComparisonValueCase"),
    ("Z", "3", "3", "true", "false", "true", "false", "true"),
    ("1", "Z", "3", "true", "false", "true", "false", "true"),
    ("1", "3", "Z", "true", "false", "true", "false", "true"),
    ("1", "3", "3", "BlaBla", "false", "true", "false", "true"),
    ("1", "3", "3", "true", "BlaBla", "true", "false", "true"),
    ("1", "3", "3", "true", "false", "BlaBla", "false", "true"),
    ("1", "3", "3", "true", "false", "true", "BlaBla", "true"),
    ("1", "3", "3", "true", "false", "true", "false", "BlaBla")
  )

  it should " return Left[ErrorMessage, FixedColumnDescription] if any of the attributes have an invalid value" in
    forAll(invalidAttributeValue) {
      (startsAt: String,
       endsAt: String,
       length: String,
       useDuringValidation: String,
       useDuringMatching: String,
       useDuringReporting: String,
       trimComparisonValue: String,
       ignoreComparisonValueCase: String) =>
        val columnDescriptionElem = <ColumnDescription
      label="this is a column in russian accent"
      startsAt={startsAt}
      endsAt={endsAt}
      length={length}
      useDuringValidation={useDuringValidation}
      useDuringMatching={useDuringMatching}
      useDuringReporting={useDuringReporting}
      trimComparisonValue={trimComparisonValue}
      ignoreComparisonValueCase={ignoreComparisonValueCase}
      />
        val columnDescription =
          FixedColumnDescription(columnDescriptionElem.attributes)
        columnDescription shouldBe a[Left[_, _]]
    }

  it should "return Left(errorMessage) if label attribute is missing" in {
    val columnDescriptionElem = <ColumnDescription
      startsAt="1"
      endsAt="3"
      />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Left[_, _]]
  }

  it should "return Left(errorMessage) if startsAt attribute is missing" in {
    val columnDescriptionElem = <ColumnDescription
      label="label"
      endsAt="3"
      />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Left[_, _]]
  }

  it should "return Left(errorMessage) if endsAt and length attribute are missing" in {
    val columnDescriptionElem = <ColumnDescription
      label="label"
      startsAt="3"
      />
    val columnDescription =
      FixedColumnDescription(columnDescriptionElem.attributes)
    columnDescription shouldBe a[Left[_, _]]
  }
}
