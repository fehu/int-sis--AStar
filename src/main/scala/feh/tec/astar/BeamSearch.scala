package feh.tec.astar

import feh.tec.astar.A_*.SortedPossibilities
import feh.util.InUnitInterval
import Ordering.Implicits._

trait BeamSearch[T] {
  self : A_*[T] =>

  def prune: BeamSearch.Prune[Heuristic, T]

  override protected def searchInnerExtraLogic: Decide = count => {
    case Some((best, opn)) => SearchInnerRecCall(best, count + 1, prune(opn))
  }


}

object BeamSearch{
  type Prune[Heuristic, T] = SortedPossibilities[Heuristic, T] => SortedPossibilities[Heuristic, T]
  type PruneTake[Heuristic, T] = Int => Prune[Heuristic, T]
  type PruneDir[Heuristic, T] = PruneTake[Heuristic, T] => Prune[Heuristic, T]

  def dropAbove[Heuristic: Ordering, T](threshold: Heuristic): Prune[Heuristic, T] = _.transform(_.filter(_._1 <= threshold))
  def dropBelow[Heuristic: Ordering, T](threshold: Heuristic): Prune[Heuristic, T] = _.transform(_.filter(_._1 >= threshold))

  def takeMax[Heuristic, T](c: Int): Prune[Heuristic, T] = _.transform(_.takeRight(c))
  def takeMin[Heuristic, T](c: Int): Prune[Heuristic, T] = _.transform(_.take(c))

  def takePercent[Heuristic, T](p: InUnitInterval)(takeBest: Int => Prune[Heuristic, T]): Prune[Heuristic, T] = {
    open =>
      val count   = (p * open.size).toInt
      if (count == 0 && open.size == 1) open
      else takeBest(count)(open)
  }
}