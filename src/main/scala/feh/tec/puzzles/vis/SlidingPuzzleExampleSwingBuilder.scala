package feh.tec.puzzles.vis

import java.awt.Color

import feh.dsl.swing.FormCreation.DSL
import feh.tec.puzzles.SlidingPuzzle.GenericSlidingPuzzleInstance
import feh.tec.puzzles.{SlidingPuzzle, SlidingPuzzleInstance}

import scala.collection.mutable
import scala.swing.Swing._
import scala.swing._
import scala.swing.event.MouseClicked

class SlidingPuzzleExampleSwingBuilder( val boardSize: (Int, Int)
                                      , val cellSize: (Int, Int)
                                      )
  extends BoxPanel(Orientation.Horizontal)
{

  lazy val initialInstBuilder  = new SlidingPuzzleInstanceSwingBuilder(boardSize, cellSize, "Initial")
  lazy val solutionInstBuilder = new SlidingPuzzleInstanceSwingBuilder(boardSize, cellSize, "Solution")

  contents += initialInstBuilder
  contents += HStrut(20)
  contents += solutionInstBuilder
}


class SlidingPuzzleInstanceSwingBuilder(boardSize: (Int, Int), cellSize: (Int, Int), instLabel: String)
  extends BoxPanel(Orientation.Vertical)
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
    updateContent()
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
  def setContent() = gPanel ++= grid.toList.sortBy(_._1).map(_._2)
  def updateContent() = {
    gPanel.clear()
    setContent()
  }
  def resetContent() = {
    grid.clear()
    grid ++= defaultGrid
    updateContent()
  }

  def listRows[R](f: Cell => R): List[List[R]] =
    grid
      .groupBy(_._1._1)
      .toList.sortBy(_._1)
      .map(_._2.toList.sortBy(_._1._2).map(f apply _._2))

  protected def defaultGrid = for {
    x <- 1 to boardSize._2
    y <- 1 to boardSize._1
    label = y + boardSize._1 * (x-1)
  } yield (x, y) -> new Cell(if (label == boardSize._1*boardSize._2) None else Some(label), x -> y)


  protected val grid = mutable.HashMap(defaultGrid: _*)

  setContent()

  protected lazy val gPanel = new GridPanel(boardSize._1, boardSize._2){
    def ++= = contents.++= _
    def clear() = contents.clear()

//    contents ++= grid.toList.sortBy(_._1).map(_._2)

    border = TitledBorder(BeveledBorder(Lowered), instLabel)
  }

  lazy val resetButton = DSL
    .triggerFor{resetContent(); gPanel.revalidate(); gPanel.repaint()}
    .button("Reset")

  contents ++= Seq(gPanel, resetButton)


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