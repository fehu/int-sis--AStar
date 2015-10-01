package feh.tec.puzzles.solve.run

import java.awt.Dimension

import feh.tec.astar.AwtHelper._
import feh.tec.astar.History
import feh.tec.puzzles._
import feh.tec.puzzles.solve.SlidingPuzzle_A_*
import feh.tec.puzzles.solve.SlidingPuzzle_A_*.{Heuristics, Solve}
import feh.tec.puzzles.vis.{FrameVisualization, GenericSlidingPuzzleAWTVisualize}
import feh.util._

import scala.language.reflectiveCalls

object SlidingPuzzles{
  def swingExampleFor[T](puzzle: SlidingPuzzle[T], initial: SlidingPuzzleInstance[T] = null) = new {
    def withSolver(solver: SlidingPuzzle_A_*[T]) = SlidingPuzzleExample.swingFrame(puzzle, Option(initial), solver)
  }
}

case class HistoryTreeShowConf(cellSize: (Int, Int), distanceBetweenH: Int, distanceBetweenV: Int){
  def dh = distanceBetweenH
  def dv = distanceBetweenV
}

object HistoryTreeShowConf{
  def default = HistoryTreeShowConf(30 -> 30, distanceBetweenH = 10, distanceBetweenV = 30)
}

case class SlidingPuzzleExample[T](puzzle: SlidingPuzzle[T],
                                   initial: Option[SlidingPuzzleInstance[T]],
                                   solver: SlidingPuzzle_A_*[T],
                                   visualize: ((Dimension => Unit) => GenericSlidingPuzzleAWTVisualize[T]) =>
                                                History[SlidingPuzzleInstance[T]] =>
                                                {def open()}
                                    )
{
  def solve = solver.search(initial getOrElse puzzle.randomInstance)
  def pVis(conf: HistoryTreeShowConf) =
    new GenericSlidingPuzzleAWTVisualize(puzzle, conf.cellSize, solver.heuristic, conf.dh, conf.dv, _: Dimension => Unit)

  def showTree(h: History[SlidingPuzzleInstance[T]], conf: HistoryTreeShowConf) = visualize(pVis(conf))(h)

  def run(conf: HistoryTreeShowConf = HistoryTreeShowConf.default) = showTree(solve._2, conf).open()
}

object SlidingPuzzleExample{
  def swingFrame[T] = SlidingPuzzleExample[T](
    _: SlidingPuzzle[T],
    _: Option[SlidingPuzzleInstance[T]],
    _: SlidingPuzzle_A_*[T],
    f => h => new FrameVisualization(f, h))
}

object SlidingPuzzleExamples{

  lazy val Example1Puzzle = new SlidingPuzzleInt3x3v2
  lazy val Example1 = SlidingPuzzles.swingExampleFor(
    Example1Puzzle,
    List(
      List(Some(2), Some(8), Some(3)),
      List(None   , Some(1), Some(4)),
      List(Some(7), Some(6), Some(5))
    ) |> (SlidingPuzzleInstance.initial(Example1Puzzle, _))
  )

  lazy val Example2Puzzle = new SlidingPuzzleInt3x3v1
  lazy val Example2 = SlidingPuzzles.swingExampleFor(
    Example2Puzzle,
    List(
      List(Some(8), Some(7), None),
      List(Some(5), Some(2), Some(6)),
      List(Some(4), Some(3), Some(1))
    ) |> (SlidingPuzzleInstance.initial(Example2Puzzle, _))
  )

}

object H{

  /** heuristic = f + g
    *        f = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum]]
    *        g = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.solutionLength]]
    */
  def _01[T] = Solve.minimizing[T, Double](x =>
    Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum(x) + Heuristics.Double.solutionLength(x)
  )


  /** heuristic = f + 0.5 * g
    *        f = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum]]
    *        g = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.solutionLength]]
    */
  def _02[T] = Solve.minimizing[T, Double](x =>
    Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum(x) + 0.5 * Heuristics.Double.solutionLength(x)
  )

}



import feh.tec.puzzles.solve.run.SlidingPuzzleExamples._

object SlidingPuzzle_Example1_H01 extends App{
  Example1.withSolver(H._01).run()
}

object SlidingPuzzle_Example1_H02 extends App{
  Example1.withSolver(H._02).run()
}

object SlidingPuzzle_Example2_H02 extends App{
  Example2.withSolver(H._02).run()
}