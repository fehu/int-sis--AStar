package feh.tec.astar

import scala.collection.immutable.{HashSet, SortedSet}

/** A* abstract algorithm.
 */
trait AStar[T] {

  type Heuristic
  type Error

  type Result = Either[T, Error]

  implicit def heuristicOrdering: Ordering[Heuristic]

  /** Searches for a solution.
   *
   * @param initial The initial state.
   * @return <code>Some(solution)</code> or <code>None</code> if the solution wasn't found.
   */
  def search(initial: T): Result

  /** Lists the next possible states.*/
  def transformations: T => Seq[T]

  def heuristic: T => Heuristic

  def isSolution: T => Boolean




  protected def error: String => Error

  protected type ExtractedOpt = Option[(T, SortedSet[T])]

  protected def extractTheBest(open: SortedSet[T]): ExtractedOpt


  // Implemented


  implicit def tparamOrdering: Ordering[T] = Ordering.by(heuristic)

  protected type Decide = PartialFunction[ExtractedOpt, Result]

  protected def searchInnerExtraLogic: Decide = Map()



  protected def searchInner(state: T,
                            count: Long,
                            open: SortedSet[T],
                            closed: HashSet[T]): Result = {
    val newStates = transformations(state)
    val newOpen   = open ++ newStates

    def decisionLogic: Decide = {
      case Some((best, _)) if isSolution(best) => Left(best)
      case Some((best, opn))                   => searchInner(best, count + 1, opn, closed + state)
      case None                                => Right(error("no solution was found in the whole search tree"))
    }

    def makeDecision = decisionLogic orElse searchInnerExtraLogic

    def extract(from: SortedSet[T]): ExtractedOpt = extractTheBest(from) match{
      case Some((best, opn)) if closed contains best => extract(opn)
      case other                                     => other
    }

    makeDecision lift extract(newOpen) getOrElse Right(error("ExtractedOpt not matched"))
  }

}
