package com.scalableQuality.quick.surface.input.errorMessages

import com.scalableQuality.quick.mantle.error.EncounteredError

object ReadFromFileErrorMessages {

  private def actionDescription(filePath: String) = s"trying to read the file at ${filePath}"

  def  cannotReadFile(filePath: String, throwable: Throwable) = {
    val errorMessage  = EncounteredError(
      actionDescription(filePath),
      s"of the following exception ${throwable.getMessage}",
      "please verify the the file exists and you have read permission"
    )
    Left(errorMessage)
  }

}
