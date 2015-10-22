package feh.tec.puzzles.vis

import java.awt.Color

import scala.collection.mutable
import scala.swing.Swing._
import scala.swing.event.MouseClicked
import scala.swing.{Frame, GridPanel, Label}

class SlidingPuzzleExampleSwingBuilder( val boardSize: (Int, Int)
                                      , val cellSize: (Int, Int)
                                      )
  extends GridPanel(1, 2)
{

  lazy val initialInstBuilder  = new SlidingPuzzleInstanceSwingBuilder(boardSize, cellSize, "Initial")
  lazy val solutionInstBuilder = new SlidingPuzzleInstanceSwingBuilder(boardSize, cellSize, "Solution")

  contents += initialInstBuilder
  contents += solutionInstBuilder

}


class SlidingPuzzleInstanceSwingBuilder(boardSize: (Int, Int), cellSize: (Int, Int), instLabel: String)
  extends GridPanel(boardSize._1, boardSize._2)
{

  class Cell(label: Any, var position: (Int, Int)) extends GridPanel(1, 1){
    border = LineBorder(Color.BLACK)

    preferredSize = cellSize
    minimumSize   = cellSize
    maximumSize   = cellSize

    contents += new Label(label.toString)
    
    reactions += {
      case _: MouseClicked => CellSelection.clicked(this)
    }

    listenTo(mouse.clicks)

    background = Color.white

    def setColor(color: Color): Unit = {
      background = color
    }
  }
  
  def swap(c1: Cell, c2: Cell) = {
    val p1 = c1.position
    c1.position = c2.position
    c2.position = p1
    putInGrid(c1, c2)
    resetContent()
    revalidate()
    repaint()
  }
  
  object CellSelection{

    var sel: Option[Cell] = None
//    var to  : Option[Cell] = None

    protected def reset(cell: Cell) = {
      sel = None
      cell.setColor(Color.white)
    }

    def clicked(cell: Cell): Unit =
      if (sel contains cell) reset(cell)
      else sel.map{
                c =>
                  swap(c, cell)
                  reset(c)
              }
              .getOrElse{
                sel = Option(cell)
                cell.setColor(Color.green.brighter)
              }

  }


  def putInGrid(cell: Cell*): Unit = grid ++= cell.map(c => c.position -> c)
  def setContent() = contents ++= grid.toList.sortBy(_._1).map(_._2)
  def resetContent() = {
    contents.clear()
    setContent()
  }

  protected val grid = mutable.HashMap((
    for {
      x <- 1 to boardSize._2
      y <- 1 to boardSize._1
      label = y + boardSize._1 * (x-1)
    } yield (x, y) -> new Cell(if (label == boardSize._1*boardSize._2) "" else label, x -> y)
    ): _*)

    setContent()

//  val panel = new GridPanel(boardSize._1, boardSize._2){
//    contents ++= grid.toList.sortBy(_._1).map(_._2)
//  }

  border = TitledBorder(BeveledBorder(Lowered), instLabel)

}

object SlidingPuzzleExampleTest extends App{

  val frame = new Frame{
    contents = new SlidingPuzzleExampleSwingBuilder(3 -> 3, 30 -> 30)
  }

  frame.open()

}