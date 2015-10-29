organization := "feh.tec"

name := "A*"

version := "0.5"

scalaVersion := "2.11.7"


mainClass in (Compile, run) := Some("feh.tec.puzzles.solve.vis.SwingConfigTst")

// Library Dependencies

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.9-SNAPSHOT"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.0"


// Test Libraries

libraryDependencies ++= List(
  "org.specs2" %% "specs2-core",
  "org.specs2" %% "specs2-html",
  "org.specs2" %% "specs2-scalacheck"
).map(_ % "3.6.4" % "test")

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"

libraryDependencies += "feh.dsl" %% "swing" % "1.5-SNAPSHOT"

// Scaladoc Options

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")

// Test Options

testOptions in Test += Tests.Argument(Some(TestFrameworks.Specs2), List("console", "html"))

initialCommands in console :=
  """ import feh.tec.puzzles.solve.run._
    | import SlidingPuzzleExamples._
  """.stripMargin