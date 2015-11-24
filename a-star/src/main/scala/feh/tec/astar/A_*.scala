package feh.tec.astar

import feh.tec.astar.A_*.MinMaxHeuristic.ExtractTheBest
import feh.util._

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.immutable.{HashSet, TreeMap}
import scala.util.{Failure, Success, Try}

 /** A* abstract algorithm.
  *
  * @tparam T state type.
  */
trait A_*[T] {
  import A_*._

  /** Return type of the heuristic(s) used. */
  type Heuristic

  /** A* result type. */
  type Result = (Try[T], History[T])

  /** An [[Ordering]] for heuristic return type. */
  implicit def heuristicOrdering: Ordering[Heuristic]

  /** Searches for a solution.
   *  Is based on [[searchInner]],
   * @param initial The initial state.
   * @return `Some(solution)` or `None` if the solution wasn't found.
   */
  def search(initial: T): Result =
    if (isSolution(initial)) Success(initial) -> HistoryRecord(HistoryEntry(initial) :: Nil)
    else runSearchInner(initial) match {
      case SearchInnerReturn(res, history) => res -> history
      case _: SearchInnerRecCall           => searchInnerRecCallEscaped_!
    }

  protected def searchInnerRecCallEscaped_! : (Try[T], History[T]) =
    implementationError("`SearchInnerRecCall` should never escape `searchInner`") -> NoHistory()

  /** Lists the next possible states.*/
  def transformations: T => Seq[T]

  /** Heuristic value for a state. */
  def heuristic: T => Heuristic

  /** Is the given state a solution? */
  def isSolution: T => Boolean

  /** A human readable description for a state. */
  def description: T => String


  /** Option: Extracted best and the rest of the [[SortedPossibilities]]. */
  protected type ExtractedOpt = Option[(T, SortedPossibilities[Heuristic, T])]

  /** Extract the best state from 'open'  [[SortedPossibilities]]]. (depends on min/max) */
  protected def extractTheBest(open: SortedPossibilities[Heuristic, T]): ExtractedOpt


  // Implemented

  /** Initiate [[searchInner]]. */
  protected def runSearchInner(initial: T) =
    searchInner(initial, 0, SortedPossibilities.empty[Heuristic, T], new HashSet, HistoryRecord(Nil))

  /** An implicit container for the heuristic. */
  implicit def heuristicContainer: HeuristicContainer[T, Heuristic] = HeuristicContainer(heuristic)

  /** Decision type: Current recursion depth => [[ExtractedOpt]] => [[SearchInnerResult]] */
  protected type Decide = Long => PartialFunction[ExtractedOpt, SearchInnerResult]

  /** Extra logic ([[Decide]]) for [[searchInner]]. Intended to be overridden. */
  protected def searchInnerExtraLogic: Decide = _ => Map()

  protected def implementationError = Failure.apply _ compose (AStarImplementationError(_: String))

  /** A container for [[searchInner]] return type.
   *  Exists because Scala cannot optimize @tailrec for f => g => f => ...
   */
  protected trait SearchInnerResult{
    def changeHistory(h: History[T]): SearchInnerResult
  }
  /** Call [[searchInner]] recursively with the specified arguments. */
  protected case class SearchInnerRecCall(state: T,
                                          count: Long,
                                          open: SortedPossibilities[Heuristic, T])           extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy()
  }
  /** Return result from [[searchInner]]. */
  protected case class SearchInnerReturn( result: Try[T], history: History[T] = NoHistory()) extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy(history = h)
  }
  protected object SearchInnerReturn{
    def create(result: Try[T]) = SearchInnerReturn(result)
  }

  protected def searchInnerError = SearchInnerReturn.create _ compose Failure.apply compose (AStarException(_: String))

  /** Part of default [[searchInner]] logic: returns a state if it's a solition. */
  protected final def caseSolution : Decide = _ => {
    case Some((best, _)) if isSolution(best) => SearchInnerReturn(Success(best))
  }
  /** Part of default [[searchInner]] logic: calls [[searchInner]] recursively. */
  protected final def recursion: Decide = count => {
    case Some((best, opn)) => SearchInnerRecCall(best, count + 1, opn)
    case None              => searchInnerError("no solution was found in the whole search tree "  ++
                                              s"($count nodes were open)"
                                              )
  }

  /** List state's parents. */
  protected def listParents: T => Seq[T]


  /** Recursive A* generic implementation.
   *  The decision logic is described by [[caseSolution]], [[searchInnerExtraLogic]] and [[recursion]], in this order.
   *
   * @param state a state to search with A*.
   * @param count recursion depth.
   * @param open  the 'open' sorted list.
   * @param closed the 'closed' set.
   * @param history previous history.
   * @return call result, based on 'decision logic'.
   */
  @tailrec
  protected final def searchInner(state: T,
                                  count: Long,
                                  open: SortedPossibilities[Heuristic, T],
                                  closed: HashSet[T],
                                  history: History[T] ): SearchInnerResult = {
    val parents   = listParents(state).toSet
    val newStates = transformations(state) filterNot parents.contains
    val newOpen   = open ++ newStates

    def extract(from: SortedPossibilities[Heuristic, T]): ExtractedOpt = extractTheBest(from) match{
      case Some((best, opn)) if closed contains best => extract(opn)
      case other                                     => other
    }

    val extracted    = extract(newOpen)
    val makeDecision = Seq(caseSolution, searchInnerExtraLogic, recursion).map(_(count)).reduceLeft(_ orElse _)

    val newHist = HistoryEntry(state, newStates.zipMap(_ => false).toMap)

    makeDecision lift extracted match {
      case Some(ret@SearchInnerReturn(res, _)) => val hist = res.map(HistoryEntry.solution(newHist :: history))
                                                                .getOrElse(newHist :: history)
                                                  ret.changeHistory(hist)
      case Some(SearchInnerRecCall(s, c, opn)) => val hist = newHist.copy(children = newHist.children.map{
                                                        case (child, _) =>
                                                          child -> (if(child == s) false else !(opn contains child)) //!(child == s || (opn contains child)) //(!(child == s) && !(opn contains child))
                                                      })
                                                  searchInner(s, c, opn, closed + state, hist :: history)
      case None                                => SearchInnerReturn(implementationError("ExtractedOpt not matched"))
      case Some(other)                         => other.changeHistory(newHist :: history)
    }
  }
}

object A_*{

  case class HeuristicContainer[T, H](h: T => H) extends (T => H){
    def apply(v: T) = h(v)
  }


  protected[astar] case class AStarException(msg: String) extends  Exception(msg){
    override def toString = "A* Exception: " + msg
  }

  protected[astar] case class AStarImplementationError(msg: String) extends RuntimeException(msg){
    override def toString = "A* Implementation Error: " + msg
  }

  object SortedPossibilities{
    def empty[H, T](implicit ord: Ordering[H], heuristic: HeuristicContainer[T, H]) =
      new SortedPossibilities(new TreeMap[H, List[T]]())
  }

  /** A class for implementing a sorted 'open' list (immutable).
    * Is implemented with [[SortedMap]], that maps heuristic values to the states with such value.
    *
    * @param underlying the [[SortedMap]].
    * @param ord        ordering for heuristic value [[H]].
    * @param heuristic  a [[HeuristicContainer]] with the heuristic used.
    * @tparam H heuristic value type.
    * @tparam T state type.
    */
  class SortedPossibilities[H, T](val underlying: SortedMap[H, List[T]])
                                 (implicit ord: Ordering[H], heuristic: HeuristicContainer[T, H])
  {

    /** Make a copy of this [[SortedPossibilities]], adding the given states. */
    def +(ts: T*) = ++(ts)

    /** Make a copy of this [[SortedPossibilities]], adding the given states. */
    def ++(ts: Traversable[T]) = new SortedPossibilities(insert(ts.toList, underlying))

    /** Insert a list of states into a [[SortedMap]]. */
    protected def insert(ts: List[T], into: SortedMap[H, List[T]]): SortedMap[H, List[T]] = ts match {
      case Nil       => into
      case t :: tail =>
        val h  = heuristic(t)
        val l  = into.get(h) map (t :: _) getOrElse List(t)
        val mp = into + (h -> l)
        insert(tail, mp)
    }

    /** Make a copy of this [[SortedPossibilities]], replacing the states with given [[H]] by the given list. */
    def replace(h: H, by: List[T]) = new SortedPossibilities( underlying + (h -> by) )
    /** Make a copy of this [[SortedPossibilities]], removing the states with given [[H]]. */
    def remove(h: H*) = new SortedPossibilities( underlying -- h )

    /** The states with the _smallest_ [[H]]. */
    def head       = underlying.head
    /** The states with the _smallest_ [[H]]. */
    def headOption = underlying.headOption
    /** Make a copy of this [[SortedPossibilities]], removing the states with _smallest_ [[H]]. */
    def tail       = new SortedPossibilities(underlying.tail)

    /** The states with the _greatest_ [[H]]. */
    def last       = underlying.last
    /** The states with the _greatest_ [[H]]. */
    def lastOption = underlying.lastOption
    /** Make a copy of this [[SortedPossibilities]], removing the states with _greatest_ [[H]]. */
    def init       = new SortedPossibilities(underlying.init)

    override def toString() = underlying.toString()

    /** Make a copy of this [[SortedPossibilities]], applying a function th the underlying [[SortedMap]]. */
    def transform(f: SortedMap[H, List[T]] => SortedMap[H, List[T]]) = new SortedPossibilities[H, T](f(underlying))

    /** The number of different [[H]] in the [[SortedPossibilities]]. */
    def size = underlying.size

    /** Contains the state? */
    def contains(t: T): Boolean = underlying.get(heuristic(t)).exists(_.contains(t))

  }

  object MinMaxHeuristic{
    protected[astar] def _extractTheBest[Heuristic, T]: (=> SortedPossibilities[Heuristic, T])
                                                        => ((Heuristic, List[T]))
                                                        => (T, SortedPossibilities[Heuristic, T]) =
      open => {
        case (h, best :: Nil) => best -> open.remove(h)
        case (h, best) =>
          val Some((chosen, rest)) = best.randomPop
          val newOpen = open.replace(h, rest.toList)
          chosen -> newOpen
      }

    type ExtractTheBest[H, T] = SortedPossibilities[H, T] => Option[(T, SortedPossibilities[H, T])]
  }

  /** Abstract A*, minimizing the heuristic. */
  trait MinimizingHeuristic[T]{
    self: A_*[T] =>

    protected def extractTheBest(open: SortedPossibilities[Heuristic, T]) = MinimizingHeuristic.extractTheBest(open)
  }

  object MinimizingHeuristic{
    /** Implementation of [[A_*.extractTheBest]] for minimizing the heuristic. */
    def extractTheBest[H, T]: ExtractTheBest[H, T] =
      open => open.headOption.map{ MinMaxHeuristic._extractTheBest(open.tail) }
  }

  /** Abstract A*, maximizing the heuristic. */
  trait MaximizingHeuristic[T] {
    self: A_*[T] =>

    protected def extractTheBest(open: SortedPossibilities[Heuristic, T]) = MaximizingHeuristic.extractTheBest(open)
  }

  object MaximizingHeuristic{
    /** Implementation of [[A_*.extractTheBest]] for maximizing the heuristic. */
    def extractTheBest[H, T]: ExtractTheBest[H, T] =
      open => open.lastOption.map{ MinMaxHeuristic._extractTheBest(open.init) }
  }

}

