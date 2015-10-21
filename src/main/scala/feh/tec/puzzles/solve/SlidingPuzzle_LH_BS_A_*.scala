package feh.tec.puzzles.solve

import SlidingPuzzle_LH_BS_A_*._
import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.astar.BeamSearch.Pnune
import feh.tec.astar.{History, BeamSearch, LimitedHorizon}
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.util.RecFunc

import scala.util.Try

/**
 * A generic mutable solver for [[feh.tec.puzzles.SlidingPuzzle]]s
 * with [[feh.tec.astar.LimitedHorizon]] and [[feh.tec.astar.BeamSearch]].
 */

class SlidingPuzzle_Mutable_LH_BS_A_*[H, Piece]( var searchDir        : SearchDirection
                                               , var heuristic        : SlidingPuzzleInstance[Piece] => H
                                               , var maxDepth         : Int
                                               , var prune            : Pnune[H, SlidingPuzzleInstance[Piece]]
                                               , var selectTheBest    : SelectTheBest[H, Piece]
                                               , var extractTheBestVar: ExtractTheBest[H, Piece]
                                               , var execSearchLimHor : ExecSearchLimHor[Piece]
                                               , var onPartialSolution: HandlePartialSolution[Piece]
                                               )
                                       (implicit val heuristicOrdering: Ordering[H])
  extends SlidingPuzzle_A_*[Piece]
  with BeamSearch                             [SlidingPuzzleInstance[Piece]]
  with LimitedHorizon                         [SlidingPuzzleInstance[Piece]]
  with LimitedHorizon.HistManagement.InMemory [SlidingPuzzleInstance[Piece]]
{
  type Heuristic = H

  protected def extractTheBest(open: SortedPossibilities[H, SlidingPuzzleInstance[Piece]]) = extractTheBestVar(open)

  protected def execSearchLH(f: RecFunc[SlidingPuzzleInstance[Piece], Result])
                            (state: SlidingPuzzleInstance[Piece]) = execSearchLimHor(f)(state)

  def handlePartialSolution(ps: PartialSolution)  = onPartialSolution(ps)


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
}


object SlidingPuzzle_LH_BS_A_*{
  object SearchDirection extends Enumeration    { val Max, Min = Value }
  type   SearchDirection = SearchDirection.Value

  type SortPoss[H, Piece] = SortedPossibilities[H, SlidingPuzzleInstance[Piece]]
  type Result[Piece] = (Try[SlidingPuzzleInstance[Piece]], History[SlidingPuzzleInstance[Piece]])

  type SelectTheBest[H, Piece] = SortPoss[H, Piece] => Map[H, Set[SlidingPuzzleInstance[Piece]]]
  type ExtractTheBest[H, Piece] = SortPoss[H, Piece] => Option[(SlidingPuzzleInstance[Piece], SortPoss[H, Piece])]

  type ExecSearchLimHor[Piece] = RecFunc[SlidingPuzzleInstance[Piece], Result[Piece]]
                              => SlidingPuzzleInstance[Piece]
                              => Result[Piece]
  type HandlePartialSolution[Piece] = LimitedHorizon[SlidingPuzzleInstance[Piece]]#PartialSolution
                                   => RecFunc.Res[SlidingPuzzleInstance[Piece], Result[Piece]]
}