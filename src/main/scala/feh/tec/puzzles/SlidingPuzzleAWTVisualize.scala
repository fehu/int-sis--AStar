package feh.tec.puzzles

import java.awt.{Color, Dimension}

import feh.tec.astar.AWTVisualizeHistory

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


abstract class GenericSlidingPuzzleAWTVisualize[Piece](val puzzle: SlidingPuzzle[Piece],
                                                       val cellSize: Dimension,
                                                       val heuristic: SlidingPuzzleInstance[Piece] => Any,
                                                       val distanceBetweenH: Int,
                                                       val distanceBetweenV: Int
                                                        )
  extends AWTVisualizeHistory[SlidingPuzzleInstance[Piece]] with SlidingPuzzleAWTVisualize[Piece]
{
  protected def depthOf = _.generation
  protected def description = _.description
}