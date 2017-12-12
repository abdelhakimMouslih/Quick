package com.scalableQuality.quick.core.fileComponentDescripts

import com.scalableQuality.quick.core.others.{ShouldUseDuring, ValueMapper}

case class ColumnDescriptionMetaData(
    position: String,
    label: String,
    shouldUseDuring: ShouldUseDuring
)
