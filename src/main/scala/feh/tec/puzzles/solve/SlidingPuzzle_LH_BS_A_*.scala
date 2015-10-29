package feh.tec.puzzles.solve

import SlidingPuzzle_LH_BS_A_*._
import akka.actor.ActorRefFactory
import feh.tec.astar.A_*.{MaximizingHeuristic, MinimizingHeuristic, SortedPossibilities}
import feh.tec.astar.BeamSearch.{PruneTake, PruneDir, Prune}
import feh.tec.astar.{History, BeamSearch, LimitedHorizon}
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.util.RecFunc

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
 * A generic mutable solver for [[feh.tec.puzzles.SlidingPuzzle]]s
 * with [[feh.tec.astar.LimitedHorizon]] and [[feh.tec.astar.BeamSearch]].
 */

abstract class SlidingPuzzle_Mutable_LH_BS_A_*[H, Piece] protected[solve] (
                                                 var heuristic        : SlidingPuzzleInstance[Piece] => H
                                               , var maxDepth         : Int
                                               , var pruneDir         : PruneDir[H, SlidingPuzzleInstance[Piece]]
                                               , var pruneTake        : PruneTake[H, SlidingPuzzleInstance[Piece]]
                                               , var selectTheBest    : SelectTheBest[H, Piece]
                                               , var extractTheBestVar: ExtractTheBest[H, Piece]
                                               )
                                       (implicit val heuristicOrdering: Ordering[H])
  extends SlidingPuzzle_A_*[Piece]
  with BeamSearch                             [SlidingPuzzleInstance[Piece]]
  with LimitedHorizon                         [SlidingPuzzleInstance[Piece]]
  with LimitedHorizon.HistManagement.InMemory [SlidingPuzzleInstance[Piece]]
{
  type Heuristic = H

  protected def extractTheBest(open: SortedPossibilities[H, SlidingPuzzleInstance[Piece]]) = extractTheBestVar(open)

  def prune: Prune[H, SlidingPuzzleInstance[Piece]] = pruneDir(pruneTake)

  private var _isRunning = false
  def isRunning = synchronized( _isRunning )

  protected def started() = synchronized( _isRunning = true )
  protected def stopped() = synchronized( _isRunning = false )

  /** Searches for a solution with limited horizon and pruning.
    *
    * @param initial The initial state.
    * @return `Some(solution)` or `None` if the solution wasn't found.
    */
  override def search(initial: SlidingPuzzleInstance[Piece]): Result =
    if (isRunning) sys.error("already running, use another instance")
    else super.search(initial)

  def execType: String
}


object SlidingPuzzle_LH_BS_A_*{
  object SearchDirection extends Enumeration    { val Max, Min = Value }
  type   SearchDirection = SearchDirection.Value

  type SortPoss[H, Piece] = SortedPossibilities[H, SlidingPuzzleInstance[Piece]]
  type Result[Piece] = (Try[SlidingPuzzleInstance[Piece]], List[History[SlidingPuzzleInstance[Piece]]])

  type SelectTheBest[H, Piece] = SortPoss[H, Piece] => Map[H, Set[SlidingPuzzleInstance[Piece]]]
  type ExtractTheBest[H, Piece] = SortPoss[H, Piece] => Option[(SlidingPuzzleInstance[Piece], SortPoss[H, Piece])]

  type ExecSearchLimHor[Piece] = RecFunc[SlidingPuzzleInstance[Piece], Result[Piece]]
                              => SlidingPuzzleInstance[Piece]
                              => Result[Piece]
  type HandlePartialSolution[Piece] = LimitedHorizon[SlidingPuzzleInstance[Piece]]#PartialSolution
                                   => RecFunc.Res[SlidingPuzzleInstance[Piece], Result[Piece]]

  class MutableContainer[H, Piece](protected val underlying: SlidingPuzzle_Mutable_LH_BS_A_*[H, Piece]){
    def affect[R](f: SlidingPuzzle_Mutable_LH_BS_A_*[H, Piece] => R): Option[R] =
      if (underlying.isRunning) None
      else Some(f(underlying))

    def heuristic = underlying.heuristic
    def maxDepth  = underlying.maxDepth
    def pruneDir  = underlying.pruneDir
    def pruneTake = underlying.pruneTake
    def selectTheBest     = underlying.selectTheBest
    def extractTheBestVar = underlying.extractTheBestVar

    def execType = underlying.execType
  }
  

  case class MutableSolverConstructor[H: Ordering, Piece]( heuristic      : SlidingPuzzleInstance[Piece] => H
                                                         , searchDir      : SearchDirection
                                                         , maxDepth       : Int
                                                         , searchDirConfig: SearchDirConfig[H, Piece]
                                                         , pruneDir       : PruneDir[H, SlidingPuzzleInstance[Piece]]
                                                         )
  {
    private lazy val setup = searchDirConfig.setup(searchDir)
    def pruneTake      = setup.pruneTake
    def selectTheBest  = setup.selectTheBest
    def extractTheBest = setup.extractTheBest

    def sequential: MutableContainer[H, Piece] =
      new MutableContainer[H, Piece](
        new SlidingPuzzle_Mutable_LH_BS_A_*[H, Piece](heuristic, maxDepth, pruneDir, pruneTake, selectTheBest, extractTheBest)
          with LimitedHorizon.Sequential[SlidingPuzzleInstance[Piece]]
        {
          def execType = "Seq"
        }
      )

    def parallel(maxExecutionTime: FiniteDuration, executorPool: Int)
                (implicit actorFactory: ActorRefFactory): MutableContainer[H, Piece] =
      new MutableContainer[H, Piece](
        new SlidingPuzzle_Mutable_LH_BS_A_*[H, Piece](heuristic, maxDepth, pruneDir, pruneTake, selectTheBest, extractTheBest)
          with LimitedHorizon.Parallel[SlidingPuzzleInstance[Piece]]
        {
          protected def aFactory = actorFactory

          def maxExecTime = maxExecutionTime
          def executorPoolSize = executorPool

          def execType = "Par"
        }
      )
  }

  case class SearchDirSetup[H, Piece]( extractTheBest: ExtractTheBest[H, Piece]
                                     , selectTheBest : SelectTheBest[H, Piece]
                                     , pruneTake     : PruneTake[H, SlidingPuzzleInstance[Piece]]
                                     )
  case class SearchDirConfig[H, Piece](setup: Map[SearchDirection, SearchDirSetup[H, Piece]])


  def defaultMinimizingSetup[H, Piece](selectBest: SelectTheBest[H, Piece]) = SearchDirSetup[H, Piece](
    extractTheBest = MinimizingHeuristic.extractTheBest,
    selectTheBest  = selectBest,
    pruneTake      = BeamSearch.takeMin
  )

  def defaultMaximizingSetup[H, Piece](selectBest: SelectTheBest[H, Piece]) = SearchDirSetup[H, Piece](
    extractTheBest = MaximizingHeuristic.extractTheBest,
    selectTheBest  = selectBest,
    pruneTake      = BeamSearch.takeMax
  )

  def defaultDirConfig[H, Piece](selectBestMax: SelectTheBest[H, Piece], selectBestMin: SelectTheBest[H, Piece]) =
    SearchDirConfig(
      Map(
        SearchDirection.Min -> defaultMinimizingSetup(selectBestMin),
        SearchDirection.Max -> defaultMaximizingSetup(selectBestMax)
      )
    )




  def setSearchDir[H, Piece](searchDir: SearchDirection, mutableSolver: SlidingPuzzle_Mutable_LH_BS_A_*[H, Piece])
                            (implicit cfg: SearchDirConfig[H, Piece]): mutableSolver.type = {
    val setup = cfg.setup(searchDir)
    mutableSolver.extractTheBestVar = setup.extractTheBest
    mutableSolver.selectTheBest = setup.selectTheBest
    mutableSolver.pruneTake = setup.pruneTake
    mutableSolver
  }


}