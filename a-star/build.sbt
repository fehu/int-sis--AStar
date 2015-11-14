organization := "feh.tec"

name := "A*"

version := "0.6"

scalaVersion := "2.11.7"


mainClass in (Compile, run) := Some("feh.tec.puzzles.solve.run.SlidingPuzzle_LH_BS_App")

// Library Dependencies

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.9-SNAPSHOT"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.0"


// Scaladoc Options

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")

