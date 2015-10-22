package feh.tec.puzzles.solve.vis

import javax.swing.SpinnerNumberModel

import feh.dsl.swing.FormCreation.DSL
import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.astar.BeamSearch
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*
import feh.tec.puzzles.solve.SlidingPuzzle_LH_BS_A_*._
import feh.tec.puzzles.solve.run.Solver
import feh.tec.puzzles.solve.vis.SlidingPuzzle_LH_BS_Solver_SwingConfig.Heuristic
import feh.util.InUnitInterval

import scala.swing.{Frame, GridPanel, Label}
import scala.util.Try


class SlidingPuzzle_LH_BS_SwingConfig {
  
  type H = Double
  
  object Solver{
//    val current = new ScopedState[MutableContainer[H, Int]]()
    
    protected lazy val builder = MutableSolverConstructor[Double, Int](
      ???, ???, ???, ???, ???
    )
    
    lazy val seq = builder.sequential
//    def par = builder.parallel _
  }
  
  
  object ConfigCommon{
    
  }
  
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

  val solverC = builder.sequential

  val frame = new Frame{
    contents = new SlidingPuzzle_LH_BS_Solver_SwingConfig(solverC, Nil)
  }

  frame.open()
}