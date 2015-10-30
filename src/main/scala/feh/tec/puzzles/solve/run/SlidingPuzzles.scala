package feh.tec.puzzles.solve.run

import feh.tec.astar.AwtHelper._
import feh.tec.astar.History
import feh.tec.puzzles._
import feh.tec.puzzles.solve.SlidingPuzzle_A_*
import feh.tec.puzzles.solve.SlidingPuzzle_A_*.{Heuristics, Solve}
import feh.tec.puzzles.vis.{FrameVisualization, GenericSlidingPuzzleAWTVisualize}
import feh.util._

object SlidingPuzzles{
  def exampleFor[T](puzzle: SlidingPuzzle[T], initial: SlidingPuzzleInstance[T] = null) = new {
    def withSolver(solver: SlidingPuzzle_A_*[T]) = SlidingPuzzleExample(puzzle, Option(initial), solver)
  }
}

case class HistoryTreeShowConf(cellSize: (Int, Int),
                               distanceBetweenH: Int,
                               distanceBetweenV: Int,
                               showRunId: Boolean ){
  def dh = distanceBetweenH
  def dv = distanceBetweenV
}

object HistoryTreeShowConf{
  def default = HistoryTreeShowConf(30 -> 30, distanceBetweenH = 10, distanceBetweenV = 30, showRunId = false)
}

case class SlidingPuzzleExample[T](puzzle: SlidingPuzzle[T],
                                   initial: Option[SlidingPuzzleInstance[T]],
                                   solver: SlidingPuzzle_A_*[T])
{
  def solve = solver.search(initial getOrElse puzzle.randomInstance)
  def showTree(conf: HistoryTreeShowConf, exitOnClose: Boolean, hs: History[SlidingPuzzleInstance[T]]*) = new FrameVisualization(
    new GenericSlidingPuzzleAWTVisualize(puzzle, conf.cellSize, solver.heuristic, conf.dh, conf.dv, _, conf.showRunId),
    exitOnClose,
    hs: _*
  )

  def run(conf: HistoryTreeShowConf = HistoryTreeShowConf.default,
          exitOnClose: Boolean = false) =
    showTree(conf, exitOnClose, solve._2).open()
}

object SlidingPuzzleExamples{

  lazy val Example1Puzzle = new SlidingPuzzleInt3x3v2
  lazy val Example1 = SlidingPuzzles.exampleFor(
    Example1Puzzle,
    List(
      List(Some(2), Some(8), Some(3)),
      List(None   , Some(1), Some(4)),
      List(Some(7), Some(6), Some(5))
    ) |> (SlidingPuzzleInstance.initial(Example1Puzzle, _))
  )

  lazy val Example2Puzzle = new SlidingPuzzleInt3x3v1
  lazy val Example2 = SlidingPuzzles.exampleFor(
    Example2Puzzle,
    List(
      List(Some(8), Some(7), None),
      List(Some(5), Some(2), Some(6)),
      List(Some(4), Some(3), Some(1))
    ) |> (SlidingPuzzleInstance.initial(Example2Puzzle, _))
  )

  lazy val Example3Puzzle = new SlidingPuzzleInt3x3v1
  lazy val Example3 = SlidingPuzzles.exampleFor(
    Example3Puzzle,
    List(
      List(Some(4), Some(1), Some(2)),
      List(None   , Some(7), Some(3)),
      List(Some(8), Some(5), Some(6))
    ) |> (SlidingPuzzleInstance.initial(Example3Puzzle, _))
  )

}

object Solver{
  object H{
    /** heuristic = f + g
      *        f = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum]]
      *        g = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.solutionLength]]
      */
    def _01[T]: SlidingPuzzleInstance[T] => Double =
      x => Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum(x) + Heuristics.Double.solutionLength(x)

    /** heuristic = f + 0.5 * g
      *        f = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum]]
      *        g = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.solutionLength]]
      */
    def _02[T]: SlidingPuzzleInstance[T] => Double =
      x => Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum(x) + 0.5 * Heuristics.Double.solutionLength(x)

    /** heuristic = f + 0.5 * g - h - k
      *        f = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum]]
      *        g = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.solutionLength]]
      *        h = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.correctlySet]]
      *        k = [[feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics.Double.correctRowsAndCols]]
      */
    def _03[T]: SlidingPuzzleInstance[T] => Double =
      x =>
        ( Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum(x)
        + 0.5 * Heuristics.Double.solutionLength(x)
        - Heuristics.Double.correctlySet(x)
        - Heuristics.Double.correctRowsAndCols(x)
        )
  }

  /** Minimizing heuristic [[H._01]]
   */
  def _01[T] = Solve.minimizing[T, Double](H._01)


  /** Minimizing heuristic [[H._02]]
    */
  def _02[T] = Solve.minimizing[T, Double](H._02)


  /** Minimizing heuristic [[H._03]]
    */
  def _03[T] = Solve.minimizing[T, Double](H._03)

}
