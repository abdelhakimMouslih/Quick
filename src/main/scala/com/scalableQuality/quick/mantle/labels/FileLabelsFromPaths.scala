package com.scalableQuality.quick.mantle.labels

import java.io.File
import java.util.regex.Pattern

import com.scalableQuality.quick.surface.commandLineOptions.QuickState

import scala.util.Try

object FileLabelsFromPaths {

  def apply(quickState: QuickState): (Option[String], Option[String]) =
    (quickState.leftFileLabel, quickState.rightFileLabel) match {
      case (Some(_), Some(_)) =>
        (quickState.leftFileLabel, quickState.rightFileLabel)
      case (None, Some(_)) =>
        val (leftFileLabel, _) =
          FileLabelsFromPaths(quickState.leftFile, quickState.rightFile)
        (leftFileLabel, quickState.rightFileLabel)

      case (Some(_), None) =>
        val (_, rightFileLabel) =
          FileLabelsFromPaths(quickState.leftFile, quickState.rightFile)
        (quickState.leftFileLabel, rightFileLabel)

      case (None, None) =>
        FileLabelsFromPaths(quickState.leftFile, quickState.rightFile)
    }

  def apply(
      leftFilePath: String,
      rightFilePath: String
  ): (Option[String], Option[String]) = {
    val leftFileCanonicalPath = getCanonicalPath(leftFilePath)
    val rightFileCanonicalPath = getCanonicalPath(rightFilePath)
    (leftFileCanonicalPath, rightFileCanonicalPath) match {
      case (Some(canonicalLeftPath), Some(canonicalRightPath)) =>
        val (leftFileDirs, leftFileName) = splitDirsFromFileName(
          canonicalLeftPath)
        val (rightFileDirs, rightFileName) = splitDirsFromFileName(
          canonicalRightPath)
        val (minifiedLeftFileDirs, minifiedRightFileDirs) =
          minifyDirs(leftFileDirs, rightFileDirs)
        val leftLabel = constructLabel(minifiedLeftFileDirs, leftFileName)
        val rightLabel = constructLabel(minifiedRightFileDirs, rightFileName)
        (Some(leftLabel), Some(rightLabel))
      case _ =>
        (None, None)
    }
  }

  private def getCanonicalPath(path: String): Option[String] = {
    val filePath = new File(path)
    Try(filePath.getCanonicalPath).toOption
  }

  private def minifyDirs(
      leftDirPath: Vector[String],
      rightDirPath: Vector[String]
  ): (Vector[String], Vector[String]) =
    if (leftDirPath.length > rightDirPath.length) {
      minifyDifferentLengthDirs(leftDirPath, rightDirPath)
    } else if (rightDirPath.length < leftDirPath.length) {
      val (minifiedRightPath, minifiedLeftPath) =
        minifyDifferentLengthDirs(rightDirPath, leftDirPath)
      (minifiedLeftPath, minifiedRightPath)
    } else {
      minifySameLengthDirs(leftDirPath, rightDirPath)
    }

  private def minifySameLengthDirs(
      leftPath: Vector[String],
      rightPath: Vector[String]
  ): (Vector[String], Vector[String]) = {
    val zippedPaths = leftPath.zip(rightPath)
    zippedPaths.dropWhile(dirs => dirs._1 == dirs._2).unzip
  }

  private def minifyDifferentLengthDirs(
      longDirPath: Vector[String],
      shortDirPath: Vector[String]
  ): (Vector[String], Vector[String]) = {

    val slicedLongPath = longDirPath.slice(0, shortDirPath.length)
    val restOfLongPath =
      longDirPath.slice(shortDirPath.length, longDirPath.length)
    val (minifiedSlicedLongPath, minifiedShortPath) =
      minifySameLengthDirs(slicedLongPath, shortDirPath)
    (minifiedSlicedLongPath ++ restOfLongPath, minifiedShortPath)
  }

  private def splitDirsFromFileName(path: String): (Vector[String], String) = {
    val splitPath = splitPathToVector(path)
    val fileName = splitPath.last
    val dirs = splitPath.slice(0, splitPath.length - 1)
    (dirs, fileName)
  }

  private def splitPathToVector(path: String): Vector[String] = {
    val literalDelimiter = Pattern.quote(pathSeparator)
    path.split(literalDelimiter, -1).toVector
  }

  private def constructLabel(dirs: Vector[String], fileName: String): String = {
    val fullPath = dirs :+ fileName
    fullPath.mkString(pathSeparator)
  }

  private val pathSeparator = File.separator
}
