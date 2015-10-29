package feh.tec.puzzles.solve.run

import feh.tec.astar.{BeamSearch, History}
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*._
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_Solver_SwingConfig.Heuristic
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_SwingConfig
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_SwingConfig.SPair
import feh.tec.puzzles.vis.SlidingPuzzleExampleSwingBuilder
import feh.tec.puzzles.{GenericSlidingPuzzle, SlidingPuzzleInstance}

import scala.swing.Frame
import scala.swing.Swing._
import scala.util.Failure

object SlidingPuzzle_LH_BS_App extends App{
  val cfg = defaultDirConfig[Double, Int](null)

  val boardSize = 3 -> 3
  val cellSize  = 30 -> 30

  val showConf = HistoryTreeShowConf.default.copy(showRunId = true)

  val builder = MutableSolverConstructor[Double, Int](
    heuristic = _ => 0,
    searchDir = SearchDirection.Min,
    maxDepth = 10,
    searchDirConfig = cfg,
    pruneDir = BeamSearch.takePercent[Double, SlidingPuzzleInstance[Int]](1)
  )

  val exampleBuilder = new SlidingPuzzleExampleSwingBuilder(boardSize, cellSize)

  val solverChooser = new SlidingPuzzle_LH_BS_SwingConfig(builder, exampleBuilder, solve, showTree)

  var lastExample: Option[SlidingPuzzleExample[Int]] = None

  def newExample = {
    val solutionRows = exampleBuilder.solutionInstBuilder.listRows(_.label)
    val puzzle = new GenericSlidingPuzzle(3, 3, 1, solutionRows){ }
    val init = exampleBuilder.initialInstBuilder.toInstance(puzzle)

    puzzle -> init
  }

  def solve: Result[Int] = {
    val (puzzle, init) = newExample
    val solver = solverChooser.currentSolver

    val resOpt = solver.get.flatMap{
      case SPair(mc, _) => mc.affect{
        solver =>
          val example = SlidingPuzzleExample[Int](puzzle, Some(init), solver)
          lastExample = Some(example)

          solver.HistoryManagement.clearHistory()
          val res  = solver.search(init)
          val hist = solver.HistoryManagement.listHistory
          res._1 -> hist
      }
    }
    resOpt getOrElse (Failure(new Exception("the solver is busy")) -> Nil)
  }

  def showTree(hist: List[History[SlidingPuzzleInstance[Int]]]): Unit =
    lastExample.foreach{ _.showTree(showConf, exitOnClose = false, hist: _*).open() }


  val frame = new Frame{
    contents = solverChooser
    size = 800 -> 500

    override def closeOperation(): Unit = sys.exit()
  }

  frame.open()
}