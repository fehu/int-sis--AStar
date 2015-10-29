package feh.tec.puzzles.solve.vis

import java.awt.Color

import feh.dsl.swing.FormCreation.DSL
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.tec.puzzles.solve.SlidingPuzzle_A_*.Heuristics

import scala.swing.Swing._
import scala.swing._
import scala.util.Try

trait HeuristicsConstructorSwing[T] extends Component{
  def heuristic: SlidingPuzzleInstance[T] => Double
}

class HeuristicsConstructorSwingImpl[T] extends GridPanel(4, 2) with HeuristicsConstructorSwing[T]{
  
  def heuristic: SlidingPuzzleInstance[T] => Double =
    x =>
      manhDistW     * Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum(x) +
      pathLengthW   * Heuristics.Double.solutionLength(x) +
      corrPiecesW   * Heuristics.Double.correctlySet(x) +
      corrColsRowsW * Heuristics.Double.correctRowsAndCols(x)

  protected var manhDistW     = 1d
  protected var pathLengthW   = 1d
  protected var corrPiecesW   = -1d
  protected var corrColsRowsW = -1d

  implicit def str2double: String => Double = java.lang.Double.valueOf

  protected def wForm(get: => Double, set: Double => Unit) = DSL
    .controlForNumeric(get)(set)
    .textForm((s: String) => Try{str2double(s)}.isSuccess)
  
  lazy val manhDistWControl     = wForm(manhDistW,     manhDistW = _)
  lazy val pathLengthWControl   = wForm(pathLengthW,   pathLengthW = _)
  lazy val corrPiecesWControl   = wForm(corrPiecesW,   corrPiecesW = _)
  lazy val corrColsRowsWControl = wForm(corrColsRowsW, corrColsRowsW = _)


  def controls = Seq(manhDistWControl, pathLengthWControl, corrPiecesWControl, corrColsRowsWControl)


  contents ++= Seq(
    "manhattan distance to solution",
    "distance from initial state",
    "# of correctly set pieces",
    "# of correctly set rows&colums"
  )
    .zip(controls)
    .flatMap{ case (l, c) => new Label(l + ":"){horizontalAlignment = Alignment.Right} :: (c: Component) :: Nil }

  controls.foreach(_.formMeta.form.updateForm())

  border = TitledBorder(LineBorder(Color.gray.brighter), "Heuristic weights")
  
}
