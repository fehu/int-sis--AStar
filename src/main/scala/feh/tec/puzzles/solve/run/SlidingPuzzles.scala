package feh.tec.puzzles.solve.run

import feh.tec.astar.AwtHelper._
import feh.tec.astar.History
import feh.tec.puzzles._
import feh.tec.puzzles.solve.SlidingPuzzle_A_*
import feh.tec.puzzles.solve.SlidingPuzzle_A_*.{Heuristics, Solve}
import feh.tec.puzzles.solve.run.SlidingPuzzleExample.Visualization
import feh.tec.puzzles.vis.{GTGEVisualization, FrameVisualization, GenericSlidingPuzzleAWTVisualize}
import feh.util._

object SlidingPuzzles{
  def exampleFor[T](puzzle: SlidingPuzzle[T], initial: SlidingPuzzleInstance[T] = null) = new {
    def withVisualization (vis: SlidingPuzzles.VisualizationArgs[T] => {def open()} = null) = new {
      def withSolver(solver: SlidingPuzzle_A_*[T]) =
        SlidingPuzzleExample(Option(vis), puzzle, Option(initial), solver)
    }

  }

  case class VisualizationArgs[T](puzzle: SlidingPuzzle[T],
                                  solver: SlidingPuzzle_A_*[T],
                                  conf: HistoryTreeShowConf,
                                  h: History[SlidingPuzzleInstance[T]])
}

case class HistoryTreeShowConf(cellSize: (Int, Int), distanceBetweenH: Int, distanceBetweenV: Int){
  def dh = distanceBetweenH
  def dv = distanceBetweenV
}

object HistoryTreeShowConf{
  def default = HistoryTreeShowConf(30 -> 30, distanceBetweenH = 10, distanceBetweenV = 30)
}

case class SlidingPuzzleExample[T](visuzlization: Option[SlidingPuzzles.VisualizationArgs[T] => {def open()}],
                                   puzzle: SlidingPuzzle[T],
                                   initial: Option[SlidingPuzzleInstance[T]],
                                   solver: SlidingPuzzle_A_*[T])
{
  def solve = solver.search(initial getOrElse puzzle.randomInstance)
  def showTree(h: History[SlidingPuzzleInstance[T]], conf: HistoryTreeShowConf) =
    visuzlization
      .map(_ apply  SlidingPuzzles.VisualizationArgs(puzzle, solver, conf, h))
      .getOrElse(new { def open() = {} })


  def run(conf: HistoryTreeShowConf = HistoryTreeShowConf.default) = showTree(solve._2, conf).open()
}

object SlidingPuzzleExample{
  object Visualization{
    def swing[T]: SlidingPuzzles.VisualizationArgs[T] => {def open()} = {
      case SlidingPuzzles.VisualizationArgs(puzzle, solver, conf, h) =>  new FrameVisualization(
        new GenericSlidingPuzzleAWTVisualize(puzzle, conf.cellSize, solver.heuristic, conf.dh, conf.dv, _),
        h
      )
    }
    def gtge[T]: SlidingPuzzles.VisualizationArgs[T] => {def open()} = {
      case SlidingPuzzles.VisualizationArgs(puzzle, solver, conf, h) =>
        new GTGEVisualization(
          new GenericSlidingPuzzleAWTVisualize(puzzle, conf.cellSize, solver.heuristic, conf.dh, conf.dv, _)
        )(h)
    }
  }
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
  Example1.withVisualization(Visualization.swing).withSolver(H._01).run()
}

object SlidingPuzzle_Example1_H02 extends App{
  Example1.withVisualization(Visualization.gtge).withSolver(H._02).run()
}

object SlidingPuzzle_Example2_H02 extends App{
  Example2.withVisualization(Visualization.swing).withSolver(H._02).run()
}