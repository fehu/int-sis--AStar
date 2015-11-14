package feh.tec.astar

import feh.tec.astar.A_*.SortedPossibilities
import feh.util.InUnitInterval
import Ordering.Implicits._

/** Abstract <b>Beam Search</b> modification of [[A_*]]. */
trait BeamSearch[T] {
  self : A_*[T] =>

  /** Prune function. */
  def prune: BeamSearch.Prune[Heuristic, T]

  override protected def searchInnerExtraLogic: Decide = count => {
    case Some((best, opn)) => SearchInnerRecCall(best, count + 1, prune(opn))
  }


}

object BeamSearch{
  /** Type of <b>prune</b> function. Drops some states from a [[SortedPossibilities]]. */
  type Prune[Heuristic, T] = SortedPossibilities[Heuristic, T] => SortedPossibilities[Heuristic, T]
  /** Prune builder: take better elements.  */
  type PruneTake[Heuristic, T] = Int => Prune[Heuristic, T]
  /** The function of direction of better items. */
  type PruneDir[Heuristic, T] = PruneTake[Heuristic, T] => Prune[Heuristic, T]

  /** Drop above the threshold */
  def dropAbove[Heuristic: Ordering, T](threshold: Heuristic): Prune[Heuristic, T] = _.transform(_.filter(_._1 <= threshold))
  /** Drop below the threshold */
  def dropBelow[Heuristic: Ordering, T](threshold: Heuristic): Prune[Heuristic, T] = _.transform(_.filter(_._1 >= threshold))

  /** [[PruneTake]] Max.*/
  def takeMax[Heuristic, T](c: Int): Prune[Heuristic, T] = _.transform(_.takeRight(c))
  /** [[PruneTake]] Min.*/
  def takeMin[Heuristic, T](c: Int): Prune[Heuristic, T] = _.transform(_.take(c))

  /** Take a % of the best states (by heuristic value). */
  def takePercent[Heuristic, T](p: InUnitInterval)(takeBest: Int => Prune[Heuristic, T]): Prune[Heuristic, T] = {
    open =>
      val count   = (math ceil p * open.size).toInt
      if (count == 0 && open.size == 1) open
      else takeBest(count)(open)
  }
}