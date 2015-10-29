package feh.tec.astar

import feh.tec.astar.HistoryEntry.Pruned

/**
 */
trait History[T] {
  def get = toList.reverse

  /** in reversed order */
  def toList: List[HistoryEntry[T]]
  def overflowed: Boolean

  def prepend(entry: HistoryEntry[T]): History[T]

  def :: = prepend _

  def map(f: HistoryEntry[T] => HistoryEntry[T]): History[T]

  def last = toList.head
  def lastOption = toList.headOption
}

case class HistoryEntry[T](parent: T, children: Map[T, Pruned] = Map.empty[T, Pruned], runId: Int = 0)

object HistoryEntry{
  type Pruned = Boolean

  def solution[T](history: History[T]): T => History[T] = history match {
    case rec@HistoryRecord(list) => SolutionHistoryRecord(list, _: T, rec.overflowed)
    case h => _ => h
  }
}

case class SolutionHistoryRecord[T](toList: List[HistoryEntry[T]], solution: T, overflowed: Boolean) extends History[T]{
  def prepend(entry: HistoryEntry[T]): History[T] = this
  def map(f: (HistoryEntry[T]) => HistoryEntry[T]): History[T] = copy(toList.map(f))
}

case class HistoryRecord[T](toList: List[HistoryEntry[T]]) extends History[T]{
  def overflowed = false
  def prepend(entry: HistoryEntry[T]) = copy(entry :: toList)
  def map(f: HistoryEntry[T] => HistoryEntry[T]) = copy(toList.map(f))
}

case class HistoryOverflow[T](toList: List[HistoryEntry[T]]) extends History[T]{
  def overflowed = true
  def prepend(entry: HistoryEntry[T]) = this
  def map(f: HistoryEntry[T] => HistoryEntry[T]) = copy(toList.map(f))
}

case class NoHistory[T]() extends History[T]{
  def overflowed = false
  def toList = Nil
  def prepend(entry: HistoryEntry[T]) = this
  def map(f: HistoryEntry[T] => HistoryEntry[T]) = this
}