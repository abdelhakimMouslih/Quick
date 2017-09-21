package com.scalableQuality.quick.core.others

sealed trait ColumnUsageStages

case object ValidationStage extends ColumnUsageStages
case object MatchingStage extends ColumnUsageStages
case object ReportingStage extends ColumnUsageStages