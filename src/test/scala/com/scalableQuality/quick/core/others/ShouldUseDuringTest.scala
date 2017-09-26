import com.scalableQuality.quick.core.others.{MatchingStage, ReportingStage, ShouldUseDuring, ValidationStage}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ShouldUseDuringTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "ShouldUseDuringTest.apply(MetaData)" should "default to false if attribute is missing" in {
    val columnDescription = <ColumnDescription />
    val shouldUseDuringEither = ShouldUseDuring(columnDescription.attributes)
    shouldUseDuringEither match {
      case Left(_) => fail

      case Right(shouldUseDuring) =>
        (shouldUseDuring(ValidationStage) || shouldUseDuring(MatchingStage) || shouldUseDuring(ReportingStage) ) shouldBe false
    }
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