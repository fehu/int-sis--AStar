name := "a-star-sliding-puzzle"

version := "0.6"

CommonSettings()

TestSettings()

mainClass in (Compile, run) := Some("feh.tec.puzzles.solve.run.SlidingPuzzle_LH_BS_App")

// Library Dependencies

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"

libraryDependencies += "feh.dsl" %% "swing" % "1.5-SNAPSHOT"



initialCommands in console :=
  """ import feh.tec.puzzles.solve.run._
    | import SlidingPuzzleExamples._
  """.stripMargin