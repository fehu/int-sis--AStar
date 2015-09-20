package feh.tec.astar

import feh.util._

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.immutable.{HashSet, TreeMap}

/** A* abstract algorithm. */
trait A_*[T] {
  import A_*._

  type Heuristic

  type Result = Either[T, Error]

  implicit def heuristicOrdering: Ordering[Heuristic]

  /** Searches for a solution.
   *
   * @param initial The initial state.
   * @return `Some(solution)` or `None` if the solution wasn't found.
   */
  def search(initial: T): Result = searchInner(initial, 0, SortedPossibilities.empty[Heuristic, T], new HashSet)

  /** Lists the next possible states.*/
  def transformations: T => Seq[T]

  def heuristic: T => Heuristic

  def isSolution: T => Boolean




  protected def error: String => Error

  protected type ExtractedOpt = Option[(T, SortedPossibilities[Heuristic, T])]

  protected def extractTheBest(open: SortedPossibilities[Heuristic, T]): ExtractedOpt


  // Implemented

  implicit def heuristicContainer: HeuristicContainer[T, Heuristic] = HeuristicContainer(heuristic)

  protected type Decide = PartialFunction[ExtractedOpt, Result]

//  protected def searchInnerExtraLogic: Decide = Map()

  var searchDebugEach: Option[Int] = None
  var searchPrintBestEach: Option[Int] = None

  @tailrec
  protected final def searchInner(state: T,
                                  count: Long,
                                  open: SortedPossibilities[Heuristic, T],
                                  closed: HashSet[T]): Result = {
    val newStates = transformations(state)
    val newOpen   = open ++ newStates

//    def decisionLogic: Decide = {
//      case Some((best, _)) if isSolution(best) => Left(best)
//      case Some((best, opn))                   => searchInner(best, count + 1, opn, closed + state)
//      case None                                => Right(error("no solution was found in the whole search tree"))
//    }

//    def makeDecision = decisionLogic orElse searchInnerExtraLogic

//    makeDecision lift extract(newOpen) getOrElse Right(error("ExtractedOpt not matched"))

    def extract(from: SortedPossibilities[Heuristic, T]): ExtractedOpt = extractTheBest(from) match{
      case Some((best, opn)) if closed contains best => extract(opn)
      case other                                     => other
    }

    val extracted = extract(newOpen)

    // print DEBUG
    searchDebugEach.withFilter(count % _ == 0) foreach (_ => println(
    s"""At search call $count:
       |  searching at $state
       |
       |  open: $open
       |  closed: $closed
       |
       |  new open: $newStates
       |  best: ${extracted.map(_._1).orNull}
       |  best heuristic: ${extracted.map(_._1 |> heuristic).orNull}
       |  resulting open: ${extracted.map(_._2).orNull}
     """.stripMargin
    ))

    // print Best
    searchPrintBestEach.withFilter(count % _ == 0) foreach (_ => println(
    s""" Reporint on call $count:
       |  best: ${extracted.map(_._1).orNull}
       |  heuristic: ${extracted.map(_._1 |> heuristic).orNull}
     """.stripMargin
    ))

    extracted match {
      case Some((best, _)) if isSolution(best) => Left(best)
      case Some((best, opn))                   => searchInner(best, count + 1, opn, closed + state)
      case None                                => Right(error("no solution was found in the whole search tree"))
    }
  }

}

object A_*{

  case class HeuristicContainer[T, H](h: T => H) extends (T => H){
    def apply(v: T) = h(v)
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
        case (_, best :: Nil) => best -> open
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

