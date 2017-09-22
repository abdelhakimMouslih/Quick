package com.scalableQuality.quick.core.others

import org.scalacheck.Prop

class ShouldUseDuringTest extends org.specs2.Specification with org.specs2.ScalaCheck {
  def is =
    s2"""
  ShouldUseDuring maps children objects of UsageStages to the booleans used to initialize it. ${expectation}
     """
  val expectation = Prop.forAll {
    (useDuringValidation: Boolean, useDuringMatching: Boolean, useDuringReporting: Boolean ) =>
      val columnUsageStage = ShouldUseDuring(useDuringValidation, useDuringMatching,useDuringReporting )
      (columnUsageStage(ValidationStage) must beEqualTo(useDuringValidation)) and
        (columnUsageStage(MatchingStage) must beEqualTo(useDuringMatching)) and
        (columnUsageStage(ReportingStage) must beEqualTo(useDuringReporting))
  }

}
