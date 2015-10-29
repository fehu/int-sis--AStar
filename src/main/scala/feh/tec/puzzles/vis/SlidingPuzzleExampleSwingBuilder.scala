package feh.tec.puzzles.vis

import java.awt.Color

import feh.tec.puzzles.SlidingPuzzle.GenericSlidingPuzzleInstance
import feh.tec.puzzles.{SlidingPuzzle, SlidingPuzzleInstance}

import scala.collection.mutable
import scala.swing.Swing._
import scala.swing.event.MouseClicked
import scala.swing._

class SlidingPuzzleExampleSwingBuilder( val boardSize: (Int, Int)
                                      , val cellSize: (Int, Int)
                                      )
  extends BoxPanel(Orientation.Horizontal)
{

  lazy val initialInstBuilder  = new SlidingPuzzleInstanceSwingBuilder(boardSize, cellSize, "Initial")
  lazy val solutionInstBuilder = new SlidingPuzzleInstanceSwingBuilder(boardSize, cellSize, "Solution")

  contents += HGlue
  contents += initialInstBuilder
  contents += HGlue
  contents += solutionInstBuilder
  contents += HGlue
}


class SlidingPuzzleInstanceSwingBuilder(boardSize: (Int, Int), cellSize: (Int, Int), instLabel: String)
  extends GridPanel(boardSize._1, boardSize._2)
{

  class Cell(val label: Option[Int], var position: (Int, Int)) extends GridPanel(1, 1){
    border = LineBorder(Color.BLACK)

    preferredSize = cellSize
    minimumSize   = cellSize
    maximumSize   = cellSize

    contents += new Label(label.mkString(""))
    
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

  def listRows[R](f: Cell => R): List[List[R]] =
    grid
      .groupBy(_._1._1)
      .toList.sortBy(_._1)
      .map(_._2.toList.sortBy(_._1._2).map(f apply _._2))

  protected val grid = mutable.HashMap((
    for {
      x <- 1 to boardSize._2
      y <- 1 to boardSize._1
      label = y + boardSize._1 * (x-1)
    } yield (x, y) -> new Cell(if (label == boardSize._1*boardSize._2) None else Some(label), x -> y)
    ): _*)

    setContent()

//  val panel = new GridPanel(boardSize._1, boardSize._2){
//    contents ++= grid.toList.sortBy(_._1).map(_._2)
//  }

  border = TitledBorder(BeveledBorder(Lowered), instLabel)


  def toInstance: SlidingPuzzle[Int] => SlidingPuzzleInstance[Int] =
    puzzle =>
      new GenericSlidingPuzzleInstance(puzzle, listRows(_.label), None, instLabel, 0)
}

object SlidingPuzzleExampleTest extends App{

  val frame = new Frame{
    contents = new SlidingPuzzleExampleSwingBuilder(3 -> 3, 30 -> 30)
  }

  frame.open()

}