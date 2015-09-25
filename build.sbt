organization := "feh.tec"

name := "A*"

version := "0.2-SNAPSHOT"

scalaVersion := "2.11.7"


// Library Dependencies

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.9-SNAPSHOT"


// Test Libraries

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" % "test"

libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.6.4"  % "test"

// Misc

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")
