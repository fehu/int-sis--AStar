package feh.tec.astar.vis

import java.awt.Point

import feh.tec.astar.{VisualizeHistory, History}
import feh.util.ScopedState

import scala.collection.mutable


abstract class AWTVisualizeHistory[T] extends VisualizeHistory[T]{
  protected val graphicsState = new ScopedState[Option[java.awt.Graphics]](None)
  protected def graphics = graphicsState.get.orNull

  def drawHistory(g: java.awt.Graphics, h: History[T]): Unit = {
    if (!historyCache.contains(h))  graphicsState.doWith(Some(g)){ drawHistoryPrepare(getHCache(h)) }
    graphicsState.doWith(Some(g)) { drawHistory(h) }
  }

  protected def drawArrow(from: Point, to: Point) = graphics.drawLine(from.x, from.y, to.x, to.y) // todo

  protected val historyCache = mutable.HashMap.empty[History[T], HistoryTree[HNode]]

  protected def getHCache(h: History[T]) = historyCache.getOrElseUpdate(h, setPositions(abstractHistoryTree(h)))

  /** Draws the history tree.
    */
  override def drawHistory(h: History[T]): Unit = drawHistory(getHCache(h))
  override def drawHistory(tr: HistoryTree[HNode]): Unit = drawHistoryRepaint(tr)
}
