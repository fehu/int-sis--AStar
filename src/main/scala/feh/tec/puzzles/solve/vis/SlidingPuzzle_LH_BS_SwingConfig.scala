package feh.tec.puzzles.solve.vis

import java.awt.Color
import java.text.DateFormat
import java.util.Calendar
import javax.swing.{ListSelectionModel, SpinnerNumberModel}

import akka.actor.ActorSystem
import feh.dsl.swing.FormCreation.DSL
import feh.dsl.swing2.{Control, Var}
import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.astar.{BeamSearch, History}
import feh.tec.puzzles.SlidingPuzzle.SlidingPuzzleInstanceOps
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*._
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_Solver_SwingConfig.Heuristic
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_SwingConfig.SPair
import feh.util._

import scala.concurrent.duration.DurationInt
import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.swing.Swing._
import scala.swing._
import scala.swing.event.ListSelectionChanged
import scala.util.{Failure, Success, Try}

/** The main graphical panel for [[feh.tec.puzzles.solve.run.SlidingPuzzle_LH_BS_App]] application. */
class SlidingPuzzle_LH_BS_SwingConfig( builder: MutableSolverConstructor[Double, Int]
                                     , extraPanel: Panel
                                     , solve: => Result[Int]
                                     , showTree: List[History[SlidingPuzzleInstance[Int]]] => Unit
                                      )
  extends GridBagPanel
{

  mainPanel =>

//  def foreachComponent(f: Component => Unit) = {
//    f(solversList.component)
//    createSolverPanel.components.foreach(f apply _.component)
//
//  }

  def lockAll(): Unit = { /* TODO */ }
  def unlockAll(): Unit = { /* TODO */ }

  val solvers = Var[List[SPair]](Nil)

  val currentSolver = Var[Option[SPair]](None)

  protected def solve_ = {
    currentSolver.get.foreach(s => s.mc.affect(_.heuristic = s.cfg.heuristic))
    solve
  }

  lazy val solversList = Control
    .custom[List[SPair], ListView[SPair]](
      solvers,
      new ListView[SPair](){ peer.setSelectionMode(ListSelectionModel.SINGLE_SELECTION) },
      initial = _ => {},
      onVarChange = lv => lv.listData = _,
      listenToUserChange = {
        lv =>
          lv.reactions += { case ListSelectionChanged(_, _, _) =>
            currentSolver set lv.selection.items.headOption
            revalidate()
            repaint()
            }
          lv.listenTo(lv.selection)
        }
    )

  lazy val createSolverPanel = new SlidingPuzzle_LH_BS_Solver_Create(builder, register)

  lazy val configPanel = new GridPanel(1, 1)
  lazy val controlPanel = new SlidingPuzzle_LH_BS_Solver_Control(
    solve_,
    showTree,
    lockAll(),
    unlockAll(),
    solutionPanel.showSolution _ andThen {_ => mainPanel.revalidate(); mainPanel.repaint() }
  )

  lazy val solutionPanel = new SlidingPuzzle_LH_BS_Solver_SwingSolution

  layout += solutionPanel -> (new Constraints $${
    c =>
      c.grid = 0 -> 0
      c.fill = Fill.Both
      c.gridheight = 2
      c.weightx = 0.4
  })

  layout += configPanel -> (new Constraints $${
    c =>
      c.grid = 1 -> 0
      c.weightx = 0.4
      c.fill = Fill.None
      c.anchor = Anchor.NorthEast
      c.insets = new Insets(10, 5, 5, 10)
  })

  layout += extraPanel -> (new Constraints $${
    c =>
      c.grid = 1 -> 1
      c.fill = Fill.None
  })

  layout += controlPanel -> (new Constraints $${
    c =>
      c.grid = 0 -> 2
      c.fill = Fill.Horizontal
      c.gridwidth = 2
  })

  layout += new ScrollPane(solversList.component){ horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
                                                   verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
    } -> (new Constraints $${
    c =>
      c.grid = 2 -> 0
      c.weightx = 0
      c.weighty = 1
      c.gridheight = 2
      c.fill = Fill.Both
  })

  layout += createSolverPanel -> (new Constraints $${
    c =>
      c.grid = 2 -> 2
      c.weighty = 0
      c.fill = Fill.Horizontal
  })

  currentSolver.onChange{
    case Some(SPair(_, cfg)) =>
      configPanel.contents.clear()
      configPanel.contents += cfg
      configPanel.revalidate()
      configPanel.repaint()
    case _ =>
  }

  def register(mc: MutableContainer[Double, Int]): Unit = {
    val spair = SPair(mc, new SlidingPuzzle_LH_BS_Solver_SwingConfig(mc, new HeuristicsConstructorSwingImpl[Int]))
    solvers.affect(_ :+ spair)
    currentSolver.set(Some(spair))
    solversList.component.selectIndices(solvers.get.indexOf(spair))
    revalidate()
    repaint()
  }
}


object SlidingPuzzle_LH_BS_SwingConfig{

  case class SPair (mc: MutableContainer[Double, Int], cfg: SlidingPuzzle_LH_BS_Solver_SwingConfig){
    override def toString = "A* LH BS " + mc.execType

  }
}

/** The control panel (buttons 'solve', 'show tree' and state) for
 * [[feh.tec.puzzles.solve.run.SlidingPuzzle_LH_BS_App]] application.
 */
class SlidingPuzzle_LH_BS_Solver_Control( solve: => Result[Int]
                                        , showTree: List[History[SlidingPuzzleInstance[Int]]] => Unit
                                        , lockAll: => Unit
                                        , unlockAll: => Unit
                                        , showResult: Try[SlidingPuzzleInstance[Int]] => Unit )
  extends GridPanel(1, 3)
{

  var lastResult: Option[Result[Int]] = None


  lazy val solveButton = DSL
    .triggerFor{
      lockAll
      setStatus("Working ...")
      val res = solve
      lastResult = Option(res)
      setStatus(res._1 map(_ => "Success") getOrElse "Failure")
      showResult(res._1)
      unlockAll
    }
    .button("Solve")

  def setStatus(s: String) = {
    statusLabel.text = s
    statusLabel.horizontalAlignment = Alignment.Center
  }

  lazy val statusLabel = new TextField(){
    editable = false
  }

  lazy val showTreeButton = DSL
    .triggerFor( lastResult foreach (showTree apply _._2) )
    .button("Show Tree")

  contents ++= Seq(solveButton, statusLabel, showTreeButton)

}

/** The solver creation panel (buttons 'sequential' and 'parallel')
 * for [[feh.tec.puzzles.solve.run.SlidingPuzzle_LH_BS_App]] application.
 */
class SlidingPuzzle_LH_BS_Solver_Create( builder: MutableSolverConstructor[Double, Int]
                                       , register: MutableContainer[Double, Int] => Unit
                                       )
  extends GridPanel(1, 2)
{

  implicit lazy val asys = ActorSystem.create("SlidingPuzzle_LH_BS_Solver")

  var execTime = 1.minute
  var execPool = 4

  lazy val createSeqButton = DSL
    .triggerFor{ register(builder.sequential) }
    .button("Sequential")

  lazy val createParButton = DSL
    .triggerFor{ register(builder.parallel(execTime, execPool)) }
    .button("Parallel")

  def components = Seq(createSeqButton, createParButton)

  contents ++= components.map(_.component)

  border = TitledBorder(BeveledBorder(Lowered), "New Solver")

}

/** A panel for configuring the selected A* instance. */
class SlidingPuzzle_LH_BS_Solver_SwingConfig( val solver: MutableContainer[Double, Int]
                                            , heuristicsConstructor: HeuristicsConstructorSwing[Int] )
  extends BoxPanel(Orientation.Vertical)
  with HeuristicsConstructorSwing[Int]
{
  
  implicit var cfg = mkCfg

  var pruneDeepSearchTakePercent: InUnitInterval = 1
  var pruneTakePercent: InUnitInterval = 1

  solver.affect(SlidingPuzzle_LH_BS_A_*.setSearchDir(SearchDirection.Min, _)) // TODO: HARDCODE !!!!


  def mkCfg = SlidingPuzzle_LH_BS_A_*.defaultDirConfig(SlidingPuzzle_LH_BS_Solver_SwingConfig.selectTheBest(pruneDeepSearchTakePercent))

  def pruneDir = BeamSearch.takePercent[Double, SlidingPuzzleInstance[Int]] _

  implicit def str2UnitInterval: String => InUnitInterval =
    s => InUnitInterval(java.lang.Double.parseDouble(s))

  def verifyInUnitInterval: String => Boolean = s => Try(str2UnitInterval(s)).isSuccess


  def heuristic = heuristicsConstructor.heuristic

  lazy val searchDirCtrl = DSL
    .controlForSeq(SearchDirection.values.toList, static = true)
    .dropDownList{
      dir =>
        solver.affect(SlidingPuzzle_LH_BS_A_*.setSearchDir(dir, _))
    }

  lazy val maxDepthCtrl = DSL
    .controlForNumeric(solver.maxDepth)(d => solver.affect(_.maxDepth = d))
    .spinner(new SpinnerNumberModel(solver.maxDepth, 1, Int.MaxValue, 1))

  lazy val prunePercentCtrl = DSL
    .controlForNumeric[InUnitInterval](1 - pruneTakePercent)(
      p => {
        pruneTakePercent = 1 - p
        solver.affect(_.pruneDir = pruneDir(pruneTakePercent))
      })
    .textForm(verifyInUnitInterval)

  lazy val bestFracThresholdCtrl = DSL
    .controlForNumeric[InUnitInterval](1 - pruneDeepSearchTakePercent)(
      p => {
        pruneDeepSearchTakePercent = 1 - p
        cfg = mkCfg
      }
    )
    .textForm(verifyInUnitInterval)

  def controls = Seq(searchDirCtrl, maxDepthCtrl, prunePercentCtrl, bestFracThresholdCtrl)

  def labels = Seq("search direction", "max. rec. depth", "prune %", "Part. sol prune %")

  protected class SolverConfig extends GridPanel(4, 2){
    for {
      (c, l) <- controls zip labels
    } {
      contents += new Label(l)
      contents += c.component
    }
  }

  contents ++= Seq(heuristicsConstructor, VGlue, new SolverConfig)

  controls.foreach(_.formMeta.form.updateForm())

  border = TitledBorder(LineBorder(Color.gray.darker()), "Configure")
}

object SlidingPuzzle_LH_BS_Solver_SwingConfig{

  case class Heuristic(name: String, value: SlidingPuzzleInstance[Int] => Double){
    override def toString = name
  }

  def selectTheBest(pruneDeepSearchTakePercent: => InUnitInterval): SortedPossibilities[Double, SlidingPuzzleInstance[Int]]
    => Map[Double, Set[SlidingPuzzleInstance[Int]]] =
  {
    sps =>
      val bestH = sps.head._1
      val take = math.floor(sps.size * pruneDeepSearchTakePercent).toInt
      sps.underlying.take(if (take == 0) 1 else take).toMap.mapValues(_.toSet)
  }

}

/** A panel for showing [[feh.tec.astar.A_*.Result]]. */
class SlidingPuzzle_LH_BS_Solver_SwingSolution extends GridPanel(1, 1){

  minimumSize   = 200 -> 100
  preferredSize = 200 -> 100

  protected def setPanel(p: Panel): Unit = {
    contents.clear()
    contents += p
  }
  
  def showSolution(res: Try[SlidingPuzzleInstance[Int]]) = res match {
    case Success(v) => setPanel(new SuccessPanel(v))
    case Failure(e) => setPanel(new FailurePanel(e))
  }
  
  protected abstract class Result extends BoxPanel(Orientation.Vertical) {
    val time = Calendar.getInstance().getTime
    
    def comp: Component
    
    contents ++= Seq(
      new Label(DateFormat.getTimeInstance.format(time)){ horizontalAlignment = Alignment.Center },
      HGlue,
      new ScrollPane(comp){
        horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
        verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded
      }
    )
  }
  
  class SuccessPanel(inst: SlidingPuzzleInstance[Int]) extends Result{
    lazy val comp = new ListView(inst.pathFromRoot)
  }
  
  class FailurePanel(ex: Throwable) extends Result{
    def err2str = Y[Throwable, String]{
      rec =>
        ex =>
          ex.getMessage + Option(ex.getCause).map(th => "\n\nCause:\n=====\n" + rec(th)).mkString("")
    }
    lazy val comp = new TextArea(err2str(ex)){
      preferredSize = 200 -> 400
      lineWrap = true
    }
  }
  
}