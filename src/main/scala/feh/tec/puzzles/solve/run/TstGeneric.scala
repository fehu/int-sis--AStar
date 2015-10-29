package feh.tec.puzzles.solve.run

import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.astar.BeamSearch
import feh.tec.puzzles.SlidingPuzzle.SlidingPuzzleInstanceOps
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*._
import feh.tec.puzzles.solve.run.SlidingPuzzleExamples._
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_Solver_SwingConfig
import feh.util.InUnitInterval

import scala.util.Success


object TstGeneric extends App{

  val bestFracThreshold: InUnitInterval = 1

  def selectTheBest(compare: (Double, Double) => Boolean): SortedPossibilities[Double, SlidingPuzzleInstance[Int]]
                                                        => Map[Double, Set[SlidingPuzzleInstance[Int]]] =
  {
    sps =>
      val bestH = sps.head._1
      val threshold = bestH * bestFracThreshold
      sps.underlying.filterKeys(compare(_, threshold)).toMap.mapValues(_.toSet)
  }


  val cfg = defaultDirConfig[Double, Int](SlidingPuzzle_LH_BS_Solver_SwingConfig.selectTheBest(1))

  val builder = MutableSolverConstructor[Double, Int](
    heuristic = Solver.H._03,
    searchDir = SearchDirection.Min,
    maxDepth = 1,
    searchDirConfig = cfg,
    pruneDir = BeamSearch.takePercent[Double, SlidingPuzzleInstance[Int]](1)
  )
  
  val solverC = builder.sequential
  
  
  solverC.affect{
    solver =>

      val exmpl  = Example3.withSolver(solver)

      val res_ = exmpl.solve
      val hist = solver.HistoryManagement.listHistory

      val showConf = HistoryTreeShowConf.default.copy(showRunId = true)
      exmpl.showTree(showConf, true, hist: _*).open()

      lazy val (Success(res), _) = res_
      res.pathFromRoot.foreach(println)
      
  }
  
  
  
  
//  val solver = new SlidingPuzzle_Mutable_LH_BS_A_*[Double, Int](
//    heuristic = Solver.H._03,
//    maxDepth = 5,
//    pruneTake = cfg.pruneTake,
//    pruneDir = BeamSearch.takePercent(0.4),
//    selectTheBest = cfg.selectTheBest,
//    extractTheBestVar = cfg.extractTheBest
//  ) with LimitedHorizon.Sequential[SlidingPuzzleInstance[Int]]
}
