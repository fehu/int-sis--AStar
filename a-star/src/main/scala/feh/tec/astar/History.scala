package feh.tec.astar

import feh.tec.astar.HistoryEntry.Pruned

/** History of A* execution.
 */
trait History[T] {
  /** Get a list of [[HistoryEntry]]. */
  def get = toList.reverse

  /** List of [[HistoryEntry]], in reversed order */
  def toList: List[HistoryEntry[T]]
  /** Has the history overflown */
  def overflowed: Boolean

  /** Make a copy of this history, prepending the given entry */
  def prepend(entry: HistoryEntry[T]): History[T]

  /** An alias for [[History.prepend]]. */
  def :: = prepend _

  /** Make a copy of this [[History]], applying the given function to each underlying [[HistoryEntry]]. */
  def map(f: HistoryEntry[T] => HistoryEntry[T]): History[T]

  /** Get the last [[HistoryEntry]]. */
  def last = toList.head
  /** Get the last [[HistoryEntry]] safely. */
  def lastOption = toList.headOption
}

/** A history entry, describing one [[A_*.searchInner]] execution.
  *
  * @param parent   the processed state.
  * @param children the children of the processed state.
  * @param runId    the id of the serie of [[A_*.searchInner]] executions.
  * @tparam T the state type.
  */
case class HistoryEntry[T](parent: T, children: Map[T, Pruned] = Map.empty[T, Pruned], runId: Int = 0)

object HistoryEntry{
  type Pruned = Boolean

  def solution[T](history: History[T]): T => History[T] = history match {
    case rec@HistoryRecord(list) => SolutionHistoryRecord(list, _: T, rec.overflowed)
    case h => _ => h
  }
}

/** An implementation of [[History]] with solution. */
case class SolutionHistoryRecord[T](toList: List[HistoryEntry[T]], solution: T, overflowed: Boolean) extends History[T]{
  def prepend(entry: HistoryEntry[T]): History[T] = this
  def map(f: (HistoryEntry[T]) => HistoryEntry[T]): History[T] = copy(toList.map(f))
}

/** The normal implementation of [[History]]. */
case class HistoryRecord[T](toList: List[HistoryEntry[T]]) extends History[T]{
  def overflowed = false
  def prepend(entry: HistoryEntry[T]) = copy(entry :: toList)
  def map(f: HistoryEntry[T] => HistoryEntry[T]) = copy(toList.map(f))
}

/** An implementation of overflown [[History]], can no longer append more entries. */
case class HistoryOverflow[T](toList: List[HistoryEntry[T]]) extends History[T]{
  def overflowed = true
  def prepend(entry: HistoryEntry[T]) = this
  def map(f: HistoryEntry[T] => HistoryEntry[T]) = copy(toList.map(f))
}

/** Disabled history. */
case class NoHistory[T]() extends History[T]{
  def overflowed = false
  def toList = Nil
  def prepend(entry: HistoryEntry[T]) = this
  def map(f: HistoryEntry[T] => HistoryEntry[T]) = this
}