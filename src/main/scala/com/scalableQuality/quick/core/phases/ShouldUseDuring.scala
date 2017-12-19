package com.scalableQuality.quick.core.phases

import com.scalableQuality.quick.core.phases.errorMessages.ShouldUseDuringErrorMessages
import com.scalableQuality.quick.mantle.constructFromXml._
import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.annotation.tailrec
import scala.xml.MetaData

class ShouldUseDuring(
    validation: Boolean,
    matching: Boolean,
    reporting: Boolean
) {
  def apply(stages: ColumnUsageStages*): Boolean = {
    @tailrec def loop(stagesList: List[ColumnUsageStages],
                      previousResult: Boolean): Boolean = stagesList match {
      case Nil =>
        previousResult

      case ValidationStage :: restOfUsages =>
        loop(restOfUsages, previousResult || validation)

      case MatchingStage :: restOfUsages =>
        loop(restOfUsages, previousResult || matching)

      case ReportingStage :: restOfUsages =>
        loop(restOfUsages, previousResult || reporting)
    }
    loop(stages.toList, false)
  }
}

object ShouldUseDuring {

  def apply(
      elemMetaData: MetaData): Either[UnrecoverableError, ShouldUseDuring] = {
    val attributeValues =
      AttributesValuesExtractor(elemMetaData, listOfAttributesKeys)

    val useDuringValidationParameterValue =
      attributeValues.get(useDuringValidationKey)
    val useDuringMatchingParameterValue =
      attributeValues.get(useDuringMatchingKey)
    val useDuringReportingParameterValue =
      attributeValues.get(useDuringReportingKey)

    val validatedParameters = validateClassParameters(
      useDuringValidationParameterValue,
      useDuringMatchingParameterValue,
      useDuringReportingParameterValue
    )

    validatedParameters match {
      case Right(
          (
            useDuringValidation,
            useDuringMatching,
            useDuringReporting
          )) =>
        Right(
          ShouldUseDuring(useDuringValidation,
                          useDuringMatching,
                          useDuringReporting))

      case Left(errorMessages) =>
        ShouldUseDuringErrorMessages.invalidAttributes(errorMessages)
    }

  }

  def apply(validation: Boolean,
            matching: Boolean,
            reporting: Boolean): ShouldUseDuring =
    new ShouldUseDuring(validation, matching, reporting)

  private def validateClassParameters(
      useDuringValidationParameterValue: Either[UnrecoverableError, Boolean],
      useDuringMatchingParameterValue: Either[UnrecoverableError, Boolean],
      useDuringReportingParameterValue: Either[UnrecoverableError, Boolean]
  ): Either[List[UnrecoverableError], (Boolean, Boolean, Boolean)] =
    (useDuringValidationParameterValue,
     useDuringMatchingParameterValue,
     useDuringReportingParameterValue) match {
      case (Right(useDuringValidation),
            Right(useDuringMatching),
            Right(useDuringReporting)) =>
        val classParameters =
          (useDuringValidation, useDuringMatching, useDuringReporting)
        Right(classParameters)
      case _ =>
        UnrecoverableError.collectAllErrors(
          useDuringMatchingParameterValue,
          useDuringValidationParameterValue,
          useDuringReportingParameterValue
        )
    }

  private val defaultUsage = Right(false)
  private val useDuringValidationKey = AttributeValueExtractor(
    "useDuringValidation",
    AttributeValueConversion.toBoolean,
    defaultUsage)
  private val useDuringMatchingKey = AttributeValueExtractor(
    "useDuringMatching",
    AttributeValueConversion.toBoolean,
    defaultUsage)
  private val useDuringReportingKey = AttributeValueExtractor(
    "useDuringReporting",
    AttributeValueConversion.toBoolean,
    defaultUsage)
  val listOfAttributesKeys =
    List(useDuringValidationKey, useDuringMatchingKey, useDuringReportingKey)
}
