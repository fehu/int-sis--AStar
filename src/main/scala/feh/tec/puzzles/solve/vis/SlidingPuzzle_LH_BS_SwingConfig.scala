package feh.tec.puzzles.solve.vis

import java.awt.Color
import javax.swing.{ListSelectionModel, SpinnerNumberModel}

import akka.actor.ActorSystem
import feh.dsl.swing.FormCreation.DSL
import feh.dsl.swing2.{Var, Control}
import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.astar.BeamSearch
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*._
import feh.tec.puzzles.solve.run.Solver
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_Solver_SwingConfig.Heuristic
import feh.util._

import scala.concurrent.duration.DurationInt
import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.swing.event.ListSelectionChanged
import scala.swing._
import scala.swing.Swing._
import scala.util.Try


class SlidingPuzzle_LH_BS_SwingConfig(heuristics: Seq[Heuristic], builder: MutableSolverConstructor[Double, Int])
  extends GridBagPanel
{

  case class SPair (mc: MutableContainer[Double, Int], cfg: SlidingPuzzle_LH_BS_Solver_SwingConfig){
    override def toString = "A* LH BS " + mc.execType
  }

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
          lv.reactions += { case ListSelectionChanged(_, _, _) => currentSolver set lv.selection.items.headOption }
          lv.listenTo(lv.selection)
        }
    )

  lazy val createSolverPanel = new SlidingPuzzle_LH_BS_Solver_Create(builder, register)

  lazy val configPanel = new GridPanel(1, 1)
  lazy val controlPanel = new GridPanel(1, 1)

  layout += configPanel -> (new Constraints $${
    c =>
      c.grid = 0 -> 0
      c.weightx = 0.8
      c.fill = Fill.None
      c.anchor = Anchor.NorthEast
      c.insets = new Insets(10, 5, 5, 10)
  })

  layout += controlPanel -> (new Constraints $${
    c =>
      c.grid = 0 -> 1
      c.fill = Fill.Horizontal
  })

  layout += solversList.component -> (new Constraints $${
    c =>
      c.grid = 1 -> 0
      c.weightx = 0
      c.weighty = 1
      c.fill = Fill.Both
  })

  layout += createSolverPanel -> (new Constraints $${
    c =>
      c.grid = 1 -> 1
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
    solvers.affect(_ :+ SPair(mc, new SlidingPuzzle_LH_BS_Solver_SwingConfig(mc, heuristics)))
  }
}

class SlidingPuzzle_LH_BS_Solver_Create( builder: MutableSolverConstructor[Double, Int]
                                       , register: MutableContainer[Double, Int] => Unit
                                       )
  extends GridPanel(1, 2)
{

  implicit lazy val asys = ActorSystem.create("SlidingPuzzle_LH_BS_Solver")

  var execTime = 1.second
  var execPool = 4

  lazy val createSeqButton = DSL
    .triggerFor{ register(builder.sequential) }
    .button("Sequential")

  lazy val createParButton = DSL
    .triggerFor{ register(builder.parallel(execTime, execPool)) }
    .button("Parallel")

  contents ++= Seq(createSeqButton, createParButton).map(_.component)

  border = TitledBorder(BeveledBorder(Lowered), "New Solver")

}

class SlidingPuzzle_LH_BS_Solver_SwingConfig( val solver: MutableContainer[Double, Int]
                                            , heuristics: Seq[Heuristic] )
  extends GridPanel(5, 2)
{
  
  implicit var cfg = mkCfg

  var bestFracThreshold: InUnitInterval = 1
  var pruneTakePercent: InUnitInterval = 1


  def mkCfg = SlidingPuzzle_LH_BS_A_*.defaultDirConfig(selectTheBest(_ >= _), selectTheBest(_ <= _))

  def pruneDir = BeamSearch.takePercent[Double, SlidingPuzzleInstance[Int]] _

  def selectTheBest(compare: (Double, Double) => Boolean): SortedPossibilities[Double, SlidingPuzzleInstance[Int]]
                                                        => Map[Double, Set[SlidingPuzzleInstance[Int]]] =
  {
    sps =>
      val bestH = sps.head._1
      val threshold = bestH * bestFracThreshold
      sps.underlying.filterKeys(compare(_, threshold)).toMap.mapValues(_.toSet)
  }

  implicit def str2UnitInterval: String => InUnitInterval =
    s => InUnitInterval(java.lang.Double.parseDouble(s))

  def verifyInUnitInterval: String => Boolean = {
    s =>
      println("verifyInUnitInterval")
      Try(str2UnitInterval(s)).isSuccess
  }


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

  lazy val pruneTakePercentCtrl = DSL
    .controlForNumeric(pruneTakePercent)(
      p => {
        pruneTakePercent = p
        solver.affect(_.pruneDir = pruneDir(p))
      })
    .textForm(verifyInUnitInterval)

  lazy val bestFracThresholdCtrl = DSL
    .controlForNumeric(bestFracThreshold)(
      p => {
        bestFracThreshold = p
        cfg = mkCfg
      }
    )
    .textForm(verifyInUnitInterval)

  def controlls = Seq(heuristicCtrl, searchDirCtrl, maxDepthCtrl, pruneTakePercentCtrl, bestFracThresholdCtrl)

  def labels = Seq("heuristic", "search dir", "max depth", "prune %", "LH best %")

  for {
    (c, l) <- controlls zip labels
  } {
    contents += new Label(l)
    contents += c.component
  }

  controlls.foreach(_.formMeta.form.updateForm())

  border = TitledBorder(LineBorder(Color.gray.darker()), "Configure")
}

object SlidingPuzzle_LH_BS_Solver_SwingConfig{
  case class Heuristic(name: String, value: SlidingPuzzleInstance[Int] => Double){
    override def toString = name
  }
}

object SwingConfigTst extends App{
  val cfg = defaultDirConfig[Double, Int](null, null)

  val builder = MutableSolverConstructor[Double, Int](
    heuristic = Solver.H._03,
    searchDir = SearchDirection.Min,
    maxDepth = 1,
    searchDirConfig = cfg,
    pruneDir = BeamSearch.takePercent[Double, SlidingPuzzleInstance[Int]](1)
  )

  val frame = new Frame{
    contents = new SlidingPuzzle_LH_BS_SwingConfig(Nil, builder)
    size = 600 -> 400
  }

  frame.open()
}