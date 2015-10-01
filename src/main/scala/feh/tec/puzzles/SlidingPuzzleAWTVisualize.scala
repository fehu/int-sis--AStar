package feh.tec.puzzles

import java.awt.Dimension

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
      val shiftY = node.position.y
      for{
        x <- 0 until puzzle.width
        y <- 0 until puzzle.height
      }{
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

    def extraHeight = 10

    /** The size of visualization.
      */
    lazy val size: Dimension = puzzle.width*cellSize.width -> (puzzle.height*cellSize.height + extraHeight)
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