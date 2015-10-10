package feh

import feh.tec.puzzles.solve.run._
import SlidingPuzzleExamples._
import scala.util._

object Tst extends App{
  val solver = Solver.LimHor._03[Int](2, 1)
  val exmpl  = Example1.withSolver(solver)

  exmpl.solve

  val hist = solver.listHistory

  val showConf = HistoryTreeShowConf.default.copy(showRunId = true)

  exmpl.showTree(showConf, true, hist: _*).open()
}
