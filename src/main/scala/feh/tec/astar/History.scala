package feh.tec.astar

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
}

case class HistoryEntry[T](parent: T, children: Set[T], runId: Int = 0)

object HistoryEntry{
  def solution[T] = HistoryEntry(_: T, Set.empty)
}

case class HistoryRecord[T](toList: List[HistoryEntry[T]]) extends History[T]{
  def overflowed = false
  def prepend(entry: HistoryEntry[T]) = HistoryRecord(entry :: toList)
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