import sbt._
import sbt.Keys._

object TestSettings{

  def apply() = Seq(
    // Test Libraries
    libraryDependencies ++= List(
      "org.specs2" %% "specs2-core"
    , "org.specs2" %% "specs2-html"
    , "org.specs2" %% "specs2-scalacheck"
    ).map(_ % "3.6.4" % "test")

//  , testOptions in Test += Tests.Argument(Some(TestFrameworks.Specs2), List("console", "html"))
  )

}
