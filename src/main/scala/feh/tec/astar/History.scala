package feh.tec.astar

/**
 */
trait History[T] {
  def toList: List[HistoryEntry[T]]
  def overflowed: Boolean

  def prepend(entry: HistoryEntry[T]): History[T]

  def :: = prepend _
}

case class HistoryEntry[T](parent: T, children: Set[T])

case class HistoryRecord[T](toList: List[HistoryEntry[T]]) extends History[T]{
  def overflowed = false
  def prepend(entry: HistoryEntry[T]) = HistoryRecord(entry :: toList)
}

case class HistoryOverflow[T](toList: List[HistoryEntry[T]]) extends History[T]{
  def overflowed = true
  def prepend(entry: HistoryEntry[T]) = this
}