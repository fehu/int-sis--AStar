package feh.tec.puzzles

import java.awt.{Graphics, Color, Dimension}
import javax.swing.{ScrollPaneConstants, JScrollPane, JPanel, JFrame}

import feh.tec.astar.{AwtHelper, History, AWTVisualizeHistory}

trait SlidingPuzzleAWTVisualize[Piece] {
  outer: AWTVisualizeHistory[SlidingPuzzleInstance[Piece]] =>

  def puzzle: SlidingPuzzle[Piece]
  def cellSize: Dimension

  lazy val drawNode = new AWTVisualizeNode

  class AWTVisualizeNode extends VisualizeNode{
    /** Draws the node.
      */
    def draw(node: HNode) = {
      val shiftX = node.position.x
      val shiftY = node.position.y + extraHeight
      for{
        x <- 0 until puzzle.width
        y <- 0 until puzzle.height
      }{
        withColor(Color.red){
          graphics.drawString(node.order.mkString, shiftX + drawNode.size.width/2, shiftY - extraHeight/2)
        }

        graphics.drawString(node.description, shiftX, shiftY-1)

        val hw = graphics.getFontMetrics.stringWidth(node.heuristic)
        graphics.drawString(node.heuristic, shiftX + drawNode.size.width - hw, shiftY-1)

        graphics.drawRect(x*cellSize.width + shiftX, y*cellSize.height + shiftY, cellSize.width, cellSize.height)

        node.state.asMap(x -> y).foreach{
          v =>
            val s = v.toString
            val sw = graphics.getFontMetrics.stringWidth(s)
            val sh = graphics.getFontMetrics.getHeight
            val sx = x*cellSize.width + cellSize.width/2 - sw / 2 + shiftX
            val sy = y*cellSize.height + cellSize.height/2 + sh / 2 + shiftY
            graphics.drawString(s, sx, sy)
        }
      }
    }

    def extraHeight = graphics.getFontMetrics.getHeight * 2

    /** The size of visualization.
      */
    lazy val size: Dimension = puzzle.width*cellSize.width -> (puzzle.height*cellSize.height + extraHeight)
  }

  protected def withColor[R](c: Color)(f: => R): R = {
    val old = graphics.getColor
    graphics.setColor(c)
    val res = f
    graphics.setColor(old)
    res
  }

}


class GenericSlidingPuzzleAWTVisualize[Piece](val puzzle: SlidingPuzzle[Piece],
                                              val cellSize: Dimension,
                                              val heuristic: SlidingPuzzleInstance[Piece] => Any,
                                              val distanceBetweenH: Int,
                                              val distanceBetweenV: Int,
                                              _setCanvasSize: Dimension => Unit
                                              )
  extends AWTVisualizeHistory[SlidingPuzzleInstance[Piece]] with SlidingPuzzleAWTVisualize[Piece]
{
  protected def depthOf = _.generation
  protected def description = _.description
  protected def setCanvasSize(dim: Dimension): Unit = _setCanvasSize(dim)
}

class FrameVisualization[T](fvh: (Dimension => Unit) => AWTVisualizeHistory[T], hist: History[T]) extends AwtHelper{
  def setCanvasSize(dim: Dimension): Unit = {
    frame.panel.setMinimumSize(dim)
    frame.panel.setPreferredSize(dim)
  }

  val vh = fvh(setCanvasSize)

  val frame: JFrame {val panel: JPanel} = new JFrame(){
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val panel = new JPanel{
      override def paint(g: Graphics): Unit = {
        vh.drawHistory(g, hist)
      }
    }
    setContentPane(panel)
    setContentPane(new JScrollPane(panel,
      ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
      ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED
    ))
  }

  def open(): Unit = {
    frame.setVisible(true)
    frame.setSize(600 -> 800)
  }
}