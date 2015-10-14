package feh

import akka.actor.ActorSystem
import feh.tec.puzzles.solve.run._
import SlidingPuzzleExamples._
import scala.util._
import scala.concurrent.duration._

object TstSeq extends App{
  val solver = Solver.LimHorSeq._03[Int](2, 1)
  val exmpl  = Example3.withSolver(solver)

  exmpl.solve

  val hist = solver.HistoryManagement.listHistory

  val showConf = HistoryTreeShowConf.default.copy(showRunId = true)

  exmpl.showTree(showConf, true, hist: _*).open()
}




object TstPar extends App{
  implicit val asys = ActorSystem("TstPar")

  val solver = Solver.LimHorPar._03[Int](2, 1, 1.minute, 5)
  val exmpl  = Example3.withSolver(solver)

  exmpl.solve

  val hist = solver.HistoryManagement.listHistory

  val showConf = HistoryTreeShowConf.default.copy(showRunId = true)

  exmpl.showTree(showConf, true, hist: _*).open()
}