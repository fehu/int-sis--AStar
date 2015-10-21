package feh.tec.astar

import feh.tec.astar.A_*.SortedPossibilities
import feh.util.InUnitInterval
import Ordering.Implicits._

trait BeamSearch[T] {
  self : A_*[T] =>

  def prune: BeamSearch.Pnune[Heuristic, T]

  override protected def searchInnerExtraLogic: Decide = count => {
    case Some((best, opn)) => SearchInnerRecCall(best, count + 1, prune(opn))
  }


}

object BeamSearch{
  type Pnune[Heuristic, T] = SortedPossibilities[Heuristic, T] => SortedPossibilities[Heuristic, T]

  def dropAbove[Heuristic: Ordering, T](threshold: Heuristic): Pnune[Heuristic, T] = _.transform(_.filter(_._1 <= threshold))
  def dropBelow[Heuristic: Ordering, T](threshold: Heuristic): Pnune[Heuristic, T] = _.transform(_.filter(_._1 >= threshold))

  def takeMax[Heuristic, T](c: Int): Pnune[Heuristic, T] = _.transform(_.takeRight(c))
  def takeMin[Heuristic, T](c: Int): Pnune[Heuristic, T] = _.transform(_.take(c))

  def dropPercent[Heuristic, T](takeBest: Int => Pnune[Heuristic, T])(p: InUnitInterval): Pnune[Heuristic, T] = {
    open =>
      val count   = (p * open.size).toInt
      if (count == 0) open
      else takeBest(count)(open)
  }
}