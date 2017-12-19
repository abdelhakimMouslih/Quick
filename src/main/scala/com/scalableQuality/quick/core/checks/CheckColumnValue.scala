package com.scalableQuality.quick.core.checks

import java.util.regex.Pattern

import com.scalableQuality.quick.core.checks.errorMessages.CheckColumnValueErrorMessages
import com.scalableQuality.quick.mantle.constructFromXml.{AttributeValueConversion, AttributeValueExtractor, AttributesValuesExtractor}
import com.scalableQuality.quick.mantle.error.UnrecoverableError

import scala.xml.MetaData

class CheckColumnValue(
    checks: List[Check]
) {
  def validate(columnValue: Option[String]): Boolean =
    checks match {
      case Nil => CheckColumnValue.noChecksResult
      case firstPreValidationFunction :: restOfPreValidationFunctions =>
        val firstPreValidationResult = firstPreValidationFunction(columnValue)
        restOfPreValidationFunctions.foldLeft(firstPreValidationResult) {
          (previousPreValidationResult, preValidationFunction) =>
            previousPreValidationResult && preValidationFunction(columnValue)
        }
    }
}

object CheckColumnValue {
  private[CheckColumnValue] val noChecksResult = true

  def apply(
      checks: List[Check]
  ): CheckColumnValue = new CheckColumnValue(checks)

  def apply(
      checks: Check*
  ): CheckColumnValue = new CheckColumnValue(checks.toList)

  def apply(metaData: MetaData): Either[UnrecoverableError, CheckColumnValue] = {
    val attributesValuesExtractor = AttributesValuesExtractor(
      metaData,
      checkColumnValueMatchRegexAttributeKey,
      checkColumnValueExistsAttributeKey)
    val checkColumnValueMatchRegexAttributeValueEither =
      attributesValuesExtractor.get(checkColumnValueMatchRegexAttributeKey)
    val checkColumnValueExistsAttributeValueEither =
      attributesValuesExtractor.get(checkColumnValueExistsAttributeKey)
    val classParameters = validateCheckColumnValueAttributes(
      checkColumnValueMatchRegexAttributeValueEither,
      checkColumnValueExistsAttributeValueEither
    )
    classParameters match {
      case Right((pattern,boolean)) =>
        val checkColumnValueMatchRegex = CheckColumnValueMatchRegex(pattern)
        val checkColumnValueExists = CheckColumnValueExists(boolean)
        Right(CheckColumnValue(checkColumnValueMatchRegex,checkColumnValueExists))
      case Left(errors) =>
        CheckColumnValueErrorMessages.invalidChecksAttributesValues(errors)
    }
  }

  private def validateCheckColumnValueAttributes(
      checkColumnValueMatchRegexAttribute: Either[UnrecoverableError, Pattern],
      checkColumnValueExistsAttribute: Either[UnrecoverableError, Boolean]
  ): Either[List[UnrecoverableError], (Pattern, Boolean)] =
    (checkColumnValueMatchRegexAttribute, checkColumnValueExistsAttribute) match {
      case (Right(pattern), Right(boolean)) =>
        Right((pattern, boolean))
      case _ =>
        UnrecoverableError.collectAllErrors(checkColumnValueMatchRegexAttribute,checkColumnValueExistsAttribute)
    }

  private val checkColumnValueMatchRegexAttributeDefaultValue = {
    val allMatchingPattern = Pattern.compile(".*")
    Right(allMatchingPattern)
  }
  private val checkColumnValueMatchRegexAttributeKey =
    AttributeValueExtractor("checkColumnValueMatches",
                            AttributeValueConversion.toPattern,
        checkColumnValueMatchRegexAttributeDefaultValue)

  private val defaultCheckColumnValueExistsAttributeValue = Right(false)
  private val checkColumnValueExistsAttributeKey = AttributeValueExtractor(
    "checkColumnValueExists",
    AttributeValueConversion.toBoolean,
    defaultCheckColumnValueExistsAttributeValue)
}
