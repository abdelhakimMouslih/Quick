package com.scalableQuality.quick.core.fileComponentDescriptions

import com.scalableQuality.quick.core.phases.ShouldUseDuring

case class ColumnDescriptionMetaData(
    position: String,
    label: String,
    shouldUseDuring: ShouldUseDuring
)
