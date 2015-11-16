import sbt._
import sbt.Keys._

object CommonSettings{

  def apply() = Seq(
    organization := "feh.tec",
    scalaVersion := "2.11.7",
    resolvers    += "Fehu's github repo" at "http://fehu.github.io/repo",
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")
  )

}