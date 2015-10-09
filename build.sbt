organization := "feh.tec"

name := "A*"

version := "0.4-SNAPSHOT"

scalaVersion := "2.11.7"


// Library Dependencies

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.9-SNAPSHOT"


// Test Libraries

libraryDependencies ++= List(
  "org.specs2" %% "specs2-core",
  "org.specs2" %% "specs2-html",
  "org.specs2" %% "specs2-scalacheck"
).map(_ % "3.6.4" % "test")

// Scaladoc Options

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")

// Test Options

testOptions in Test += Tests.Argument(Some(TestFrameworks.Specs2), List("console", "html"))

initialCommands in console :=
  """ import feh.tec.puzzles.solve.run._
    | import SlidingPuzzleExamples._
  """.stripMargin