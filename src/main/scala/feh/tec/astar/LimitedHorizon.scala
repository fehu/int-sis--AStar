package feh.tec.astar

import feh.tec.astar.A_*.{AStarException, SortedPossibilities}
import feh.util._

import scala.collection.immutable.TreeMap
import scala.util.{Failure, Success, Try}

trait LimitedHorizon[T] {
  self : A_*[T] =>

  def maxDepth: Int

  def selectTheBest: SortedPossibilities[Heuristic, T] => Map[Heuristic, Set[T]]



  def handlePartialSolution(ps: PartialSolution): RecFunc.Res[T, (Try[T], History[T])]

  protected def execSearchLH(f: RecFunc[T, Result])(state: T): Result

  case class PartialSolution(best: Set[T])

  case class PartialSolutionReturn(result: Try[PartialSolution],
                                   history: History[T] = NoHistory()) extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy(history = h)
  }


  override protected def searchInnerExtraLogic: Decide =
    count => {
      case Some((best, open)) if count == maxDepth =>
        val bestSel = selectTheBest  apply (open + best)
        PartialSolutionReturn(Success(PartialSolution(bestSel.values.toSet.flatten)))
    }



  protected def searchLH = RecFunc[T, Result]{ t => runSearchInner(t) match {
    case SearchInnerReturn(res, history) =>
      RecFunc Ret (res -> history)
    case PartialSolutionReturn(Success(ps), history) =>
      handlePartialSolution(ps)
    case PartialSolutionReturn(fail, history) =>
      RecFunc Ret fail.asInstanceOf[Failure[T]] -> history
    case _: SearchInnerRecCall =>
      RecFunc Ret implementationError("`SearchInnerRecCall` should never escape `searchInner`") -> NoHistory() // todo: duplication!
  }}

  /** Searches for a solution with limited horizon.
    *
    * @param initial The initial state.
    * @return `Some(solution)` or `None` if the solution wasn't found.
    */
  override def search(initial: T) = execSearchLH(searchLH)(initial)


}


object LimitedHorizon{
  protected trait PartialSolutionsBuffer[T]{
    self : A_*[T] with LimitedHorizon[T] =>

    protected var buffer = SortedPossibilities.empty[Heuristic, T]

    def guardPartialSolution(ps: PartialSolution): Unit = { buffer ++= ps.best }

    def popBestPartialSolution(): Option[T] = extractTheBest(buffer) map {
      case (best, rest) =>
        buffer = rest
        best
    }

  }


  trait Sequential[T] extends LimitedHorizon[T] with PartialSolutionsBuffer[T] {
    self : A_*[T] =>

    def handlePartialSolution(ps: PartialSolution) = {
      guardPartialSolution(ps)
      popBestPartialSolution()
        .map(RecFunc.Rec.apply)
        .getOrElse(RecFunc.Ret(Failure(AStarException("no more partial solutions to search")) -> NoHistory()))
    }

    protected def execSearchLH(f: RecFunc[T, Result])(state: T): Result = RecFunc.TCO(state, f)
  }

  /** TODO: implement
   */
  trait Parallel[T] extends LimitedHorizon[T]{
    self: A_*[T] =>


  }
}