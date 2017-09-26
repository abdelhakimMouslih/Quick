package com.scalableQuality.quick.core.others

import com.scalableQuality.quick.mantle.buildFromXml._
import com.scalableQuality.quick.mantle.log.{ErrorMessage, UnrecoverableError}

import scala.annotation.tailrec
import scala.xml.MetaData

class ShouldUseDuring (
                       validation: Boolean,
                       matching: Boolean,
                       reporting: Boolean
                        ) {
  def apply(stages: ColumnUsageStages*): Boolean = {
    @tailrec def loop(stagesList: List[ColumnUsageStages], previousResult: Boolean): Boolean =  stagesList match {
      case Nil =>
        previousResult

      case ValidationStage::restOfUsages =>
        loop(restOfUsages, previousResult || validation)

      case MatchingStage::restOfUsages =>
        loop(restOfUsages, previousResult || matching)

      case ReportingStage::restOfUsages =>
        loop(restOfUsages, previousResult || reporting)
    }
    loop(stages.toList, false)
  }
}

object ShouldUseDuring {

  def apply(elemMetaData: MetaData): Either[ErrorMessage, ShouldUseDuring] = {
    val classParameters:XMLAttributesToClassParameters =
      XMLAttributesToClassParameters(elemMetaData, shouldUseDuringKeys)
    val useDuringValidationParameterValue = classParameters.safeGet(useDuringValidationKey)
    val useDuringMatchingParameterValue = classParameters.safeGet(useDuringMatchingKey)
    val useDuringReportingParameterValue = classParameters.safeGet(useDuringReportingKey)
    val parametersValidated = validateClassParameters(
      useDuringValidationParameterValue,
      useDuringMatchingParameterValue,
      useDuringReportingParameterValue
    )
    parametersValidated match {
      case Right((
        useDuringValidation,
        useDuringMatching,
        useDuringReporting
        )) => Right(ShouldUseDuring(useDuringValidation,useDuringMatching,useDuringReporting))
      case Left(errorMessage) => cannotMakeShouldUseDuring(errorMessage)
    }
  }

  def apply(validation: Boolean, matching:Boolean, reporting: Boolean ): ShouldUseDuring =
    new ShouldUseDuring(validation, matching, reporting)

  private def validateClassParameters(
                                       useDuringValidationParameterValue: ParameterValueFound[Boolean],
                                       useDuringMatchingParameterValue : ParameterValueFound[Boolean],
                                       useDuringReportingParameterValue : ParameterValueFound[Boolean]
                                     ): Either[ErrorMessage, (Boolean, Boolean, Boolean)] =
    (useDuringValidationParameterValue, useDuringMatchingParameterValue, useDuringReportingParameterValue) match {
      case (
        ValidParameterValueFound(useDuringValidation),
        ValidParameterValueFound(useDuringMatching),
        ValidParameterValueFound(useDuringReporting)
        ) =>
        Right((useDuringValidation,useDuringMatching, useDuringReporting))
      case (InvalidParameterValueFound(errorMessage), _, _) => Left(errorMessage)
      case (_, InvalidParameterValueFound(errorMessage), _) => Left(errorMessage)
      case (_, _, InvalidParameterValueFound(errorMessage)) => Left(errorMessage)
  }

  private def cannotMakeShouldUseDuring(errorMessage: ErrorMessage): Either[ErrorMessage, ShouldUseDuring] = Left(
    UnrecoverableError(
      "specifying at which stages the column should be used",
      "one of useDuring attributes contains a invalid boolean value",
      "fix the problem described below",
      List(errorMessage)
    )
  )

  private val defaultUsage = false
  private val useDuringValidationKey = ParameterAttributeWithDefaultValue("useDuringValidation",AttributeConversionFunctions.toBoolean, defaultUsage)
  private val useDuringMatchingKey = ParameterAttributeWithDefaultValue("useDuringMatching",AttributeConversionFunctions.toBoolean,defaultUsage)
  private val useDuringReportingKey = ParameterAttributeWithDefaultValue("useDuringReporting",AttributeConversionFunctions.toBoolean,defaultUsage)
  private val shouldUseDuringKeys = List(useDuringValidationKey, useDuringMatchingKey, useDuringReportingKey )
}