package feh.tec.puzzles.vis

import java.awt.event.KeyEvent
import java.awt.{Graphics2D, Dimension}

import com.golden.gamedev.{GameLoader, Game}
import feh.tec.astar.{AwtHelper, History}
import feh.tec.astar.vis.AWTVisualizeHistory


class GTGEVisualization [T](fvh: (Dimension => Unit) => AWTVisualizeHistory[T])(hist: History[T]) extends AwtHelper{
  private var cSize: Dimension = null
  def setCanvasSize(d: Dimension): Unit = cSize = (d.width*1.01).toInt -> (d.height*1.01).toInt

  lazy val vh = fvh(setCanvasSize)
  lazy val tr = vh.setPositions(vh.abstractHistoryTree(hist))

  lazy val game = new Game {
    def initResources(): Unit = {
      setFPS(100)
    }
    def update(l: Long): Unit = {
      if (keyPressed(KeyEvent.VK_ESCAPE)) finish()
    }
    def render(graphics2D: Graphics2D): Unit = vh.withGraphics(graphics2D){ vh.drawHistoryRepaint(tr) }
  }

  def open() = {
    vh.drawHistoryPrepare(tr)
    val loader = new GameLoader()
    loader.setup(game, cSize, false)
    loader.start()
  }

}
