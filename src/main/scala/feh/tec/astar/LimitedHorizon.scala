package feh.tec.astar

import feh.tec.astar.A_*.SortedPossibilities
import feh.util._

import scala.util.{Failure, Success, Try}

trait LimitedHorizon[T] {
  self : A_*[T] =>

  def maxDepth: Int

  def selectTheBest: SortedPossibilities[Heuristic, T] => Map[Heuristic, Set[T]]



  def withNewPartialSolution(ps: PartialSolution): TCO.Res[T, (Try[T], History[T])]



  case class PartialSolution(best: Map[Heuristic, Set[T]])

  case class PartialSolutionReturn(result: Try[PartialSolution],
                                   history: History[T] = NoHistory()) extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy(history = h)
  }


  override protected def searchInnerExtraLogic: Decide =
    count => {
      case Some((best, open)) if count == maxDepth =>
        val bestSel = selectTheBest  apply (open + best)
        PartialSolutionReturn(Success(PartialSolution(bestSel)))
    }

  /** Searches for a solution with limited horizon.
    *
    * @param initial The initial state.
    * @return `Some(solution)` or `None` if the solution wasn't found.
    */
  override def search(initial: T) = TCO[T, Result](initial, {
    case SearchInnerReturn(res, history) =>
      TCO Ret (res -> history)
    case PartialSolutionReturn(Success(ps), history) =>
      withNewPartialSolution(ps)
    case PartialSolutionReturn(fail, history) =>
      TCO Ret fail.asInstanceOf[Failure[T]] -> history
    case _: SearchInnerRecCall =>
      TCO Ret implementationError("`SearchInnerRecCall` should never escape `searchInner`") -> NoHistory() // todo: duplication!
  })


//  {
//    runSearchInner(initial) match {
//      case SearchInnerReturn(res, history) =>
//        res -> history
//      case PartialSolutionReturn(Success(ps), history) =>
//        withNewPartialSolution(ps)
//      case PartialSolutionReturn(fail, history) =>
//        fail.asInstanceOf[Failure[T]] -> history
//      case _: SearchInnerRecCall =>
//        implementationError("`SearchInnerRecCall` should never escape `searchInner`") -> NoHistory() // todo: duplication!
//    }
//  }


  protected object TCO{
    sealed trait Res[+A, +B]
    case class Rec[A](a: A) extends Res[A, Nothing]
    case class Ret[B](b: B) extends Res[Nothing, B]

    def apply[A, B](initial: A, f: A => Res[A, B]): B = {
      var flag = true
      var arg  = initial
      var res: B = null.asInstanceOf[B]

      while(flag)
        f(arg) match {
          case Rec(a) => arg = a
          case Ret(b) => res = b; flag = false
        }

      res
    }
  }

}
