package feh.tec.astar

import feh.util._

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.immutable.{HashSet, TreeMap}
import scala.util.{Failure, Success, Try}

/** A* abstract algorithm. */
trait A_*[T] {
  import A_*._

  type Heuristic

  type Result = Try[T]

  implicit def heuristicOrdering: Ordering[Heuristic]

  /** Searches for a solution.
   *
   * @param initial The initial state.
   * @return `Some(solution)` or `None` if the solution wasn't found.
   */
  def search(initial: T): Result = searchInner(initial, 0, SortedPossibilities.empty[Heuristic, T], new HashSet) match {
    case SearchInnerReturn(res) => res
    case _: SearchInnerRecCall  => Failure(new Exception("`SearchInnerRecCall` should never escape `searchInner`"))
  }

  /** Lists the next possible states.*/
  def transformations: T => Seq[T]

  def heuristic: T => Heuristic

  def isSolution: T => Boolean


  protected type ExtractedOpt = Option[(T, SortedPossibilities[Heuristic, T])]

  protected def extractTheBest(open: SortedPossibilities[Heuristic, T]): ExtractedOpt


  // Implemented

  implicit def heuristicContainer: HeuristicContainer[T, Heuristic] = HeuristicContainer(heuristic)

  protected type Decide = Long => PartialFunction[ExtractedOpt, SearchInnerResult]

  protected def searchInnerExtraLogic: Decide = _ => Map()

  var searchDebugEach: Option[Int] = None
  var searchPrintBestEach: Option[Int] = None

  /** Exists because Scala cannot optimize @tailrec for f => g => f => ... */
  protected trait SearchInnerResult
  protected case class SearchInnerRecCall(state: T,
                                          count: Long,
                                          open: SortedPossibilities[Heuristic, T]) extends SearchInnerResult
  protected case class SearchInnerReturn( result: Result                         ) extends SearchInnerResult

  protected def searchInnerError = SearchInnerReturn.apply _ compose Failure.apply compose (new AStarException(_: String))

  protected final def decisionLogic: Decide = count => {
    case Some((best, _)) if isSolution(best) => SearchInnerReturn(Success(best))
    case Some((best, opn))                   => SearchInnerRecCall(best, count + 1, opn)
    case None                                => searchInnerError("no solution was found in the whole search tree")
  }


  @tailrec
  protected final def searchInner(state: T,
                                  count: Long,
                                  open: SortedPossibilities[Heuristic, T],
                                  closed: HashSet[T]): SearchInnerResult = {
    val newStates = transformations(state)
    val newOpen   = open ++ newStates


    def makeDecision = searchInnerExtraLogic(count) orElse decisionLogic(count)

    def extract(from: SortedPossibilities[Heuristic, T]): ExtractedOpt = extractTheBest(from) match{
      case Some((best, opn)) if closed contains best => extract(opn)
      case other                                     => other
    }

    val extracted = Try{extract(newOpen)}

    //    _printEach(count, state, open, closed, newStates, extracted.toOption.flatten, extracted.map(_._1 |> heuristic).orNull)

    extracted map makeDecision.lift match {
      case Success(Some(res@SearchInnerReturn(_)))      => res
      case Success(Some(SearchInnerRecCall(s, c, opn))) => searchInner(s, c, opn, closed + state)
      case Success(None)                                => searchInnerError("ExtractedOpt not matched")
      case Failure(fail)                                => SearchInnerReturn(Failure(fail))
    }
}


//  protected def _printEach(count: Long, state: Any, open: Any, closed: Any,
//                           newStates: Any, extracted: Option[(Any, Any)], bestHeuristic: Any) =
//  {
//    // print DEBUG
//    searchDebugEach.withFilter(count % _ == 0) foreach (_ => println(
//      s"""At search call $count:
//                                 |  searching at $state
//          |
//          |  open: $open
//          |  closed: $closed
//          |
//          |  new open: $newStates
//          |  best: ${extracted.map(_._1).orNull}
//          |  best heuristic: $bestHeuristic
//          |  resulting open: ${extracted.map(_._2).orNull}
//     """.stripMargin
//    ))
//
//    // print Best
//    searchPrintBestEach.withFilter(count % _ == 0) foreach (_ => println(
//      s""" Reporint on call $count:
//                                    |  best: ${extracted.map(_._1).orNull}
//          |  heuristic: $bestHeuristic
//     """.stripMargin
//    ))
//  }
}

object A_*{

  case class HeuristicContainer[T, H](h: T => H) extends (T => H){
    def apply(v: T) = h(v)
  }


  protected case class AStarException(msg: String) extends  Exception(msg){
    override def toString = "A* Exception: " + msg
  }

  protected case class AStarImplementationError(msg: String) extends RuntimeException(msg){
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

    def ++(ts: Seq[T]) = new SortedPossibilities(insert(ts.toList, underlying))

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

  }

  protected trait MinMaxHeuristic[T]{
    self: A_*[T] =>

    def _extractTheBest: SortedPossibilities[Heuristic, T] => ((Heuristic, List[T])) => (T, SortedPossibilities[Heuristic, T]) =
      open => {
        case (h, best :: Nil) => best -> open.remove(h)
        case (h, best) =>
          val Some((chosen, rest)) = best.randomPop
          val newOpen = open.replace(h, rest.toList)
          chosen -> newOpen
      }
  }

  trait MinimizingHeuristic[T] extends MinMaxHeuristic[T]{
    self: A_*[T] =>

    protected def extractTheBest(open: SortedPossibilities[Heuristic, T]) = open.headOption.map{ _extractTheBest(open.tail) }
  }

  trait MaximizingHeuristic[T] extends MinMaxHeuristic[T]{
    self: A_*[T] =>

    protected def extractTheBest(open: SortedPossibilities[Heuristic, T]) = open.lastOption.map{ _extractTheBest(open.init) }
  }

}

