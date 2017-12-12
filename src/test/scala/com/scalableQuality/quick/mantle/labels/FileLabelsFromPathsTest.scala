package com.scalableQuality.quick.mantle.labels

import java.io.File

import com.scalableQuality.quick.surface.commandLineOptions.QuickState
import org.scalatest.{FlatSpec, Matchers}

class FileLabelsFromPathsTest extends FlatSpec with Matchers {

  val pathSeparator = File.separator
  def systemDependentPath(names: String*): String = names.mkString(pathSeparator)

  "FileLabelsFromPaths.apply(leftFilePath, rightFilePath)" should "return only the name of the file when two relative paths are the same" in {
    val path = systemDependentPath("dir", "subDir", "file")
    val expectedResult = (Some("file"), Some("file"))
    FileLabelsFromPaths(path, path) shouldBe expectedResult
  }

  it should "ignore ./ and return the files name when their parent directory is the same" in {
    val leftFilePath = systemDependentPath("dir", "subDir", "file")
    val rightFilePath = systemDependentPath(".", "dir", "subDir", "differentFile")
    val expectedResult = (Some("file"), Some("differentFile"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "remove the common parent directory from the two relative paths" in {
    val leftFilePath = systemDependentPath("dir", "subDir", "file")
    val rightFilePath = systemDependentPath("dir", "differentSubDir", "file")
    val expectedResult = (Some(systemDependentPath("subDir", "file")), Some(systemDependentPath("differentSubDir", "file")))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "understand ../ and removed common parent directory from the two relative paths" in {
    val leftFilePath = systemDependentPath("..", "dir", "subDir", "file")
    val rightFilePath = systemDependentPath("..", "dir", "subDir", "differentFile")
    val expectedResult = (Some("file"), Some("differentFile"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "treat ../dir... and ./dir as different directories" in {
    val leftFilePath = systemDependentPath("..", "dir", "subDir", "file")
    val rightFilePath = systemDependentPath(".", "dir", "subDir", "file")
    val expectedLeftFilePath = Some(systemDependentPath("dir", "subDir", "file"))
    val (obtainedLeftFilePath, obtainedRightFilePath) = FileLabelsFromPaths(leftFilePath, rightFilePath)
    obtainedLeftFilePath should (be (expectedLeftFilePath) and not be (obtainedRightFilePath))
  }

  "FileLabelsFromPaths.apply(quickState)" should "return labels provided in quickState" in {
    val quickState = QuickState(
      leftFileLabel = Some("leftLabel"),
      leftFile = systemDependentPath("leftDir","leftFile"),
      rightFileLabel = Some("rightLabel"),
      rightFile = systemDependentPath("rightDir","rightFile")
    )
    val expectedResult = (quickState.leftFileLabel,quickState.rightFileLabel)
    FileLabelsFromPaths(quickState) shouldBe expectedResult
  }

  it should "return a minified left file path and right label if only the right label is provided" in {
    val quickState = QuickState(
      leftFile = systemDependentPath("leftDir","leftFile"),
      rightFileLabel = Some("rightLabel"),
      rightFile = systemDependentPath("rightDir","rightFile")
    )
    val expectedResult = (Some(quickState.leftFile),quickState.rightFileLabel)
    FileLabelsFromPaths(quickState) shouldBe expectedResult
  }

  it should "return a minified right file path and left label if only the left label is provided" in {
    val quickState = QuickState(
      leftFileLabel = Some("leftLabel"),
      leftFile = systemDependentPath("leftDir","leftFile"),
      rightFile = systemDependentPath("rightDir","rightFile")
    )
    val expectedResult = (quickState.leftFileLabel,Some(quickState.rightFile))
    FileLabelsFromPaths(quickState) shouldBe expectedResult
  }

  it should "return a minified left and right paths if no labels were provided" in {
    val quickState = QuickState(
      leftFile = systemDependentPath("leftDir","leftFile"),
      rightFile = systemDependentPath("rightDir","rightFile")
    )
    val expectedResult = (Some(quickState.leftFile),Some(quickState.rightFile))
    FileLabelsFromPaths(quickState) shouldBe expectedResult
  }

}
