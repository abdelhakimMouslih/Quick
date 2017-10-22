// project specifications
name := "Quick"
version := "0.2"

// fat JAR specifications
mainClass in assembly := Some("com.scalableQuality.quick.surface.main.Quick")
assemblyJarName in assembly := "quick.jar"

// compiler options
scalaVersion := "2.11.11"
scalacOptions ++= Seq(
  "-target:jvm-1.6",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-feature"
)

// import libraries
// adding scopt, a CLI options parsing library
libraryDependencies ++= Seq("com.github.scopt" %% "scopt" % "3.6.0" )
// adding scala.xml, scala standard xml parsing library
libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if Scala 2.12+ is used, use scala-swing 2.x
    case Some((2, scalaMajor)) if scalaMajor >= 12 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
        "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2")
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
        "org.scala-lang.modules" %% "scala-swing" % "1.0.2")
    case _ =>
      // or just libraryDependencies.value if you don't depend on scala-swing
      libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
  }
}

// adding ScalaTest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

// adding ScalaCheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

