organization := "feh.tec"

name := "A*-root"

publishArtifact := false

scalaVersion := "2.11.7"

lazy val root = project in file(".") aggregate (aStar, slidingPuzzle)

lazy val aStar = project in file("a-star")

lazy val slidingPuzzle = project in file("sliding-puzzle") dependsOn aStar