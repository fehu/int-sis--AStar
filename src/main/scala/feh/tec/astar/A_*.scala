package feh.tec.astar

import feh.tec.astar.A_*.MinMaxHeuristic.ExtractTheBest
import feh.util._

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.immutable.{HashSet, TreeMap}
import scala.util.{Failure, Success, Try}

/** A* abstract algorithm. */
trait A_*[T] {
  import A_*._

  type Heuristic

  type Result = (Try[T], History[T])

  implicit def heuristicOrdering: Ordering[Heuristic]

  /** Searches for a solution.
   *
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

  def heuristic: T => Heuristic

  def isSolution: T => Boolean

  def description: T => String


  protected type ExtractedOpt = Option[(T, SortedPossibilities[Heuristic, T])]

  protected def extractTheBest(open: SortedPossibilities[Heuristic, T]): ExtractedOpt


  // Implemented

  protected def runSearchInner(initial: T) =
    searchInner(initial, 0, SortedPossibilities.empty[Heuristic, T], new HashSet, HistoryRecord(Nil))

  implicit def heuristicContainer: HeuristicContainer[T, Heuristic] = HeuristicContainer(heuristic)

  protected type Decide = Long => PartialFunction[ExtractedOpt, SearchInnerResult]

  protected def searchInnerExtraLogic: Decide = _ => Map()

  protected def implementationError = Failure.apply _ compose (AStarImplementationError(_: String))

  /** Exists because Scala cannot optimize @tailrec for f => g => f => ... */
  protected trait SearchInnerResult{
    def changeHistory(h: History[T]): SearchInnerResult
  }
  protected case class SearchInnerRecCall(state: T,
                                          count: Long,
                                          open: SortedPossibilities[Heuristic, T])           extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy()
  }
  protected case class SearchInnerReturn( result: Try[T], history: History[T] = NoHistory()) extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy(history = h)
  }
  protected object SearchInnerReturn{
    def create(result: Try[T]) = SearchInnerReturn(result)
  }

  protected def searchInnerError = SearchInnerReturn.create _ compose Failure.apply compose (AStarException(_: String))

  protected final def caseSolution : Decide = _ => {
    case Some((best, _)) if isSolution(best) => SearchInnerReturn(Success(best))
  }
  
  protected final def recursion: Decide = count => {
    case Some((best, opn)) => SearchInnerRecCall(best, count + 1, opn)
    case None              => searchInnerError("no solution was found in the whole search tree "  ++
                                              s"($count nodes were open)"
                                              )
  }

  protected def listParents: T => Seq[T]


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

    val newHist = HistoryEntry(state, newStates.zipMap(_ => false).toMap) :: history

    makeDecision lift extracted match {
      case Some(ret@SearchInnerReturn(res, _)) => val hist = res.map(HistoryEntry.solution(newHist))
                                                                .getOrElse(newHist)
                                                  ret.changeHistory(hist)
      case Some(SearchInnerRecCall(s, c, opn)) => val hist = newHist.map{
                                                    entry =>
                                                      entry.copy(children = entry.children.map{
                                                        case (child, _) => child -> (opn contains child)
                                                      })
                                                  }
                                                  searchInner(s, c, opn, closed + state, hist)
      case None                                => SearchInnerReturn(implementationError("ExtractedOpt not matched"))
      case Some(other)                         => other.changeHistory(newHist)
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

  class SortedPossibilities[H, T](val underlying: SortedMap[H, List[T]])
                                 (implicit ord: Ordering[H], heuristic: HeuristicContainer[T, H])
  {

    def +(ts: T*) = ++(ts)

    def ++(ts: Traversable[T]) = new SortedPossibilities(insert(ts.toList, underlying))

    protected def insert(ts: List[T], into: SortedMap[H, List[T]]): SortedMap[H, List[T]] = ts match {
      case Nil       => into
      case t :: tail =>
        val h  = heuristic(t)
        val l  = into.get(h) map (t :: _) getOrElse List(t)
        val mp = into + (h -> l)
        insert(tail, mp)
    }

    def replace(h: H, by: List[T]) = new SortedPossibilities( underlying + (h -> by) )
    def remove(h: H*) = new SortedPossibilities( underlying -- h )

    def head       = underlying.head
    def headOption = underlying.headOption
    def tail       = new SortedPossibilities(underlying.tail)

    def last       = underlying.last
    def lastOption = underlying.lastOption
    def init       = new SortedPossibilities(underlying.init)

    override def toString() = underlying.toString()

    def transform(f: SortedMap[H, List[T]] => SortedMap[H, List[T]]) = new SortedPossibilities[H, T](f(underlying))

    def size = underlying.size

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

  trait MinimizingHeuristic[T]{
    self: A_*[T] =>

    protected def extractTheBest(open: SortedPossibilities[Heuristic, T]) = MinimizingHeuristic.extractTheBest(open)
  }

  object MinimizingHeuristic{
    def extractTheBest[H, T]: ExtractTheBest[H, T] =
      open => open.headOption.map{ MinMaxHeuristic._extractTheBest(open.tail) }
  }


  trait MaximizingHeuristic[T] {
    self: A_*[T] =>

    protected def extractTheBest(open: SortedPossibilities[Heuristic, T]) = MaximizingHeuristic.extractTheBest(open)
  }

  object MaximizingHeuristic{
    def extractTheBest[H, T]: ExtractTheBest[H, T] =
      open => open.lastOption.map{ MinMaxHeuristic._extractTheBest(open.init) }
  }

}

