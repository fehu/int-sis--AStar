package feh.tec.puzzles.solve.vis

import java.awt.Color
import javax.swing.{ListSelectionModel, SpinnerNumberModel}

import akka.actor.ActorSystem
import feh.dsl.swing.FormCreation.DSL
import feh.dsl.swing2.ComponentExt.ComponentWrapper
import feh.dsl.swing2.{Var, Control}
import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.astar.{NoHistory, History, BeamSearch}
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_SwingConfig.SPair
import feh.tec.puzzles.{GenericSlidingPuzzle, SlidingPuzzleInstance}
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*._
import feh.tec.puzzles.solve.run.{SlidingPuzzleExample, HistoryTreeShowConf, Solver}
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_Solver_SwingConfig.Heuristic
import feh.tec.puzzles.vis.SlidingPuzzleExampleSwingBuilder
import feh.util._

import scala.concurrent.duration.DurationInt
import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.swing.event.ListSelectionChanged
import scala.swing._
import scala.swing.Swing._
import scala.util.{Success, Failure, Try}


class SlidingPuzzle_LH_BS_SwingConfig( heuristics: Seq[Heuristic]
                                     , builder: MutableSolverConstructor[Double, Int]
                                     , extraPanel: Panel
                                     , solve: => Result[Int]
                                     , showTree: List[History[SlidingPuzzleInstance[Int]]] => Unit
                                      )
  extends GridBagPanel
{

//  def foreachComponent(f: Component => Unit) = {
//    f(solversList.component)
//    createSolverPanel.components.foreach(f apply _.component)
//
//  }

  def lockAll(): Unit = { /* TODO */ }
  def unlockAll(): Unit = { /* TODO */ }

  val solvers = Var[List[SPair]](Nil)

  val currentSolver = Var[Option[SPair]](None)

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
  lazy val controlPanel = new SlidingPuzzle_LH_BS_Solver_Control(solve, showTree, lockAll(), unlockAll())

  layout += configPanel -> (new Constraints $${
    c =>
      c.grid = 0 -> 0
      c.weightx = 0.8
      c.fill = Fill.None
      c.anchor = Anchor.NorthEast
      c.insets = new Insets(10, 5, 5, 10)
  })

  layout += extraPanel -> (new Constraints $${
    c =>
      c.grid = 0 -> 1
      c.fill = Fill.Horizontal
  })

  layout += controlPanel -> (new Constraints $${
    c =>
      c.grid = 0 -> 2
      c.fill = Fill.Horizontal
  })

  layout += solversList.component -> (new Constraints $${
    c =>
      c.grid = 1 -> 0
      c.weightx = 0
      c.weighty = 1
      c.gridheight = 2
      c.fill = Fill.Both
  })

  layout += createSolverPanel -> (new Constraints $${
    c =>
      c.grid = 1 -> 2
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
    val spair = SPair(mc, new SlidingPuzzle_LH_BS_Solver_SwingConfig(mc, heuristics))
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

class SlidingPuzzle_LH_BS_Solver_Control( solve: => Result[Int]
                                        , showTree: List[History[SlidingPuzzleInstance[Int]]] => Unit
                                        , lockAll: => Unit
                                        , unlockAll: => Unit)
  extends GridPanel(1, 3)
{

  var lastResult: Option[Result[Int]] = None


  lazy val solveButton = DSL
    .triggerFor{
      lockAll
      setStatus("Working ...")
      println("Working ...")
      val res = solve
      lastResult = Option(res)
      setStatus(res._1 map(_ => "Success") getOrElse "")
      res._1.failed.foreach(ex => Dialog.showMessage(null, ex.getMessage, "ERROR", Dialog.Message.Error))
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

class SlidingPuzzle_LH_BS_Solver_SwingConfig( val solver: MutableContainer[Double, Int]
                                            , heuristics: Seq[Heuristic] )
  extends GridPanel(5, 2)
{
  
  implicit var cfg = mkCfg

  var pruneDeepSearchTakePercent: InUnitInterval = 1
  var pruneTakePercent: InUnitInterval = 0

  solver.affect(SlidingPuzzle_LH_BS_A_*.setSearchDir(SearchDirection.Min, _)) // TODO: HARDCODE !!!!


  def mkCfg = SlidingPuzzle_LH_BS_A_*.defaultDirConfig(SlidingPuzzle_LH_BS_Solver_SwingConfig.selectTheBest(pruneDeepSearchTakePercent))

  def pruneDir = BeamSearch.takePercent[Double, SlidingPuzzleInstance[Int]] _

  implicit def str2UnitInterval: String => InUnitInterval =
    s => InUnitInterval(java.lang.Double.parseDouble(s))

  def verifyInUnitInterval: String => Boolean = s => Try(str2UnitInterval(s)).isSuccess


  lazy val heuristicCtrl = DSL
    .controlForSeq(heuristics, static = true)
    .dropDownList(h => solver.affect(_.heuristic = h.value))

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
    .controlForNumeric(pruneTakePercent)(
      p => {
        pruneTakePercent = 1 - p
        solver.affect(_.pruneDir = pruneDir(pruneTakePercent))
      })
    .textForm(verifyInUnitInterval)

  lazy val bestFracThresholdCtrl = DSL
    .controlForNumeric(pruneDeepSearchTakePercent)(
      p => {
        pruneDeepSearchTakePercent = p
        cfg = mkCfg
      }
    )
    .textForm(verifyInUnitInterval)

  def controls = Seq(heuristicCtrl, searchDirCtrl, maxDepthCtrl, prunePercentCtrl, bestFracThresholdCtrl)

  def labels = Seq("heuristic", "search dir", "max depth", "prune %", "LH best %")

  for {
    (c, l) <- controls zip labels
  } {
    contents += new Label(l)
    contents += c.component
  }

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
