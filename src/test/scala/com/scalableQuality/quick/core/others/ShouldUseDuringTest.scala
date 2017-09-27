import com.scalableQuality.quick.core.others.{MatchingStage, ReportingStage, ShouldUseDuring, ValidationStage}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers}

class ShouldUseDuringTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with TableDrivenPropertyChecks {
  "ShouldUseDuringTest.apply(MetaData)" should "default to false if attribute is missing" in {
    val columnDescription = <ColumnDescription />
    val shouldUseDuringEither = ShouldUseDuring(columnDescription.attributes)
    shouldUseDuringEither match {
      case Left(_) => fail

      case Right(shouldUseDuring) =>
        (shouldUseDuring(ValidationStage) || shouldUseDuring(MatchingStage) || shouldUseDuring(ReportingStage) ) shouldBe false
    }
  }

  it should "should handle upper case boolean values" in forAll {
    (useDuringValidation: Boolean, useDuringMatching: Boolean, useDuringReporting: Boolean) =>
      val columnDescription = <ColumnDescription
        useDuringValidation={useDuringValidation.toString.toUpperCase}
        useDuringMatching={useDuringMatching.toString.toUpperCase}
        useDuringReporting={useDuringReporting.toString.toUpperCase} />
      val shouldUseDuringEither = ShouldUseDuring(columnDescription.attributes)
      shouldUseDuringEither match {
        case Left(_) => fail

        case Right(shouldUseDuring) =>
          assert(
            shouldUseDuring(ValidationStage) === useDuringValidation &&
              shouldUseDuring(MatchingStage) === useDuringMatching &&
              shouldUseDuring(ReportingStage) === useDuringReporting
          )
      }
  }

  val invalidBooleans = Table(
    ("useDuringValidation","useDuringMatching","useDuringMatching"),
    ("blabla", "true", "true"),
    ("true", "blabla", "true"),
    ("true", "true", "blabla")
  )

  it should "return Left[ErrorMessage, ShouldUseDuring] if any of the attribute have an invald boolean" in
    forAll(invalidBooleans) { (useDuringValidation: String, useDuringMatching:String, useDuringReporting: String) =>
      val columnDescription = <ColumnDescription
        useDuringValidation={useDuringValidation}
        useDuringMatching={useDuringMatching}
        useDuringReporting={useDuringReporting} />
      val shouldUseDuringEither = ShouldUseDuring(columnDescription.attributes)
      shouldUseDuringEither shouldBe a [Left[_,_]]
    }

  "ShouldUseDuringTest.apply(ColumnUsageStages*)" should "map its boolean input to the appropriate ColumnUsageStage" in forAll {
    (useDuringValidation: Boolean, useDuringMatching: Boolean, useDuringReporting: Boolean) =>
      val columnDescription = <ColumnDescription
        useDuringValidation={useDuringValidation.toString}
        useDuringMatching={useDuringMatching.toString}
        useDuringReporting={useDuringReporting.toString} />
      val shouldUseDuringEither = ShouldUseDuring(columnDescription.attributes)
      shouldUseDuringEither match {
        case Left(_) => fail

        case Right(shouldUseDuring) =>
          assert(
            shouldUseDuring(ValidationStage) === useDuringValidation &&
              shouldUseDuring(MatchingStage) === useDuringMatching &&
              shouldUseDuring(ReportingStage) === useDuringReporting
          )
      }
  }

  it should "return true if the column is used in at least one of the ColumnUsageStages passed as arguments" in forAll {
    (useDuringValidation: Boolean, useDuringMatching: Boolean, useDuringReporting: Boolean) =>
      val columnDescription = <ColumnDescription
        useDuringValidation={useDuringValidation.toString}
        useDuringMatching={useDuringMatching.toString}
        useDuringReporting={useDuringReporting.toString} />
      val shouldUseDuringEither = ShouldUseDuring(columnDescription.attributes)
      shouldUseDuringEither match {
        case Left(_) => fail

        case Right(shouldUseDuring) =>
          shouldUseDuring(ValidationStage,MatchingStage,ReportingStage) shouldBe (useDuringValidation || useDuringMatching || useDuringReporting)
      }
  }


}