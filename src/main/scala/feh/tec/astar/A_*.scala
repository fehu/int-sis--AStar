package feh.tec.astar

import scala.collection.SortedMap
import scala.collection.immutable.{HashSet, SortedSet}

/** A* abstract algorithm. */
trait A_*[T] {
  import A_*._

  type Heuristic
  type Error

  type Result = Either[T, Error]

  implicit def heuristicOrdering: Ordering[Heuristic]

  /** Searches for a solution.
   *
   * @param initial The initial state.
   * @return `Some(solution)` or `None` if the solution wasn't found.
   */
  def search(initial: T): Result

  /** Lists the next possible states.*/
  def transformations: T => Seq[T]

  implicit def heuristic: T => Heuristic

  def isSolution: T => Boolean




  protected def error: String => Error

  protected type ExtractedOpt = Option[(T, SortedPossibilities[Heuristic, T])]

  protected def extractTheBest(open: SortedPossibilities[Heuristic, T]): ExtractedOpt


  // Implemented

  protected type Decide = PartialFunction[ExtractedOpt, Result]

  protected def searchInnerExtraLogic: Decide = Map()



  protected def searchInner(state: T,
                            count: Long,
                            open: SortedPossibilities[Heuristic, T],
                            closed: HashSet[T]): Result = {
    val newStates = transformations(state)
    val newOpen   = open ++ newStates

    def decisionLogic: Decide = {
      case Some((best, _)) if isSolution(best) => Left(best)
      case Some((best, opn))                   => searchInner(best, count + 1, opn, closed + state)
      case None                                => Right(error("no solution was found in the whole search tree"))
    }

    def makeDecision = decisionLogic orElse searchInnerExtraLogic

    def extract(from: SortedPossibilities[Heuristic, T]): ExtractedOpt = extractTheBest(from) match{
      case Some((best, opn)) if closed contains best => extract(opn)
      case other                                     => other
    }

    makeDecision lift extract(newOpen) getOrElse Right(error("ExtractedOpt not matched"))
  }

}

object A_*{

  class SortedPossibilities[H, T](val underlying: SortedMap[H, List[T]])
                                 (implicit ord: Ordering[H], heuristic: T => H)
  {

    def +(ts: T*) = ++(ts)

    def ++(ts: Seq[T]) = new SortedPossibilities(insert(ts.toList, underlying))

    def insert(ts: List[T], into: SortedMap[H, List[T]]): SortedMap[H, List[T]] = ts match {
      case Nil       => into
      case t :: tail =>
        val h  = heuristic(t)
        val l  = into.get(h) map (t :: _) getOrElse List(t)
        val mp = into + (h -> l)
        insert(tail, mp)
    }

    def head       = underlying.head
    def headOption = underlying.headOption
    def tail       = underlying.tail

    def last       = underlying.last
    def lastOption = underlying.lastOption
    def init       = underlying.init

  }

}

