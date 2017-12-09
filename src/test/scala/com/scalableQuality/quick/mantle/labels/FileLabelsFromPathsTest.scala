package com.scalableQuality.quick.mantle.labels

import org.scalatest.{FlatSpec, Matchers}

class FileLabelsFromPathsTest extends FlatSpec with Matchers {

  "FileLabelsFromPaths.apply()" should "return only the name of the file when two paths are the same" in {
    val path = "/stuff/home/file"
    val expectedResult = (Some("file"), Some("file"))
    FileLabelsFromPaths(path, path) shouldBe expectedResult
  }

  it should "return the two file names if they're in the same absolute dir " in {
    val leftFilePath = "/home/gandalf/beard/grey"
    val rightFilePath = "/home/gandalf/beard/white"
    val expectedResult = (Some("grey"), Some("white"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "return the two file names if they're in the same relative dir " in {
    val leftFilePath = "home/gandalf/beard/grey"
    val rightFilePath = "./home/gandalf/beard/white"
    val expectedResult = (Some("grey"), Some("white"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "return the two file names if no parents dirs are provided" in {
    val leftFilePath = "grey"
    val rightFilePath = "white"
    val expectedResult = (Some("grey"), Some("white"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "return the two file names if one file is reference directly and the other from the current dir" in {
    val leftFilePath = "grey"
    val rightFilePath = "./white"
    val expectedResult = (Some("grey"), Some("white"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "remove the common parents dir from the two absolute files paths" in {
    val leftFilePath = "/home/gandalf/hair/grey"
    val rightFilePath = "/home/gandalf/beard/white"
    val expectedResult = (Some("hair/grey"), Some("beard/white"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "remove the common parents dir from the two relative files paths" in {
    val leftFilePath = "home/gandalf/hair/grey"
    val rightFilePath = "./home/gandalf/beard/white"
    val expectedResult = (Some("hair/grey"), Some("beard/white"))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

  it should "return the same absolute paths if they are completely different " in {
    val leftFilePath = "/home/gandalf/hair/grey"
    val rightFilePath = "/home/gandalf/hair/grey/home/gandalf/hair/grey"
    val expectedResult = (Some(leftFilePath), Some(rightFilePath))
    FileLabelsFromPaths(leftFilePath, rightFilePath) shouldBe expectedResult
  }

}
