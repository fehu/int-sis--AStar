package feh.tec.rubik.solve

import feh.tec.astar.{HistoryRecord, History, A_*}
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.solve.RubikCubeHeuristics.DistanceMeasure._
import feh.tec.rubik.{RubikCube, RubikCubeInstance}
import feh.tec.rubik.solve.RubikCubeHeuristics.{DistanceMeasure, DistanceMeasureOLD, SomeTricks}
import feh.util._

import scala.collection.mutable
import scala.util.Success

/** Implements A* methods related to [[RubikCubeInstance]]s.
  */
trait RubikCube_A_*[T] extends A_*[RubikCubeInstance[T]]{
  /** Lists the next possible states. */
  def transformations = c => SideName.values.toSeq.map(c.rotate)

  /** Is the given state a solution? */
  def isSolution = inst => heuristic(inst) == solutionLengthHeuristic(inst)
//    _.cubeById.forall{ case (_, CubeWithOrientation(c, o)) => c.labels == o.toSeq }

  /** List state's parents. */
  def listParents: RubikCubeInstance[T] => Seq[RubikCubeInstance[T]] = c => c.parent match {
    case Some(p) => p +: listParents(p)
    case _       => Stream.empty
  }

  /** A human readable description for a state. */
  def description = _.description.toString

  def fullDescription: RubikCubeInstance[T] => RubikCube.Description = _.description

  def solutionLengthHeuristic: RubikCubeInstance[T] => Heuristic

}




object RubikCube_A_*{

  /**
    */
  class WithTricksStages[T: WithSideName](val stages: Seq[RubikCubeHeuristics.SomeTricks.Stage],
                                          val concentratedSearchDepth: Int,
                                          implicit val measureFunc: MeasureFunc[T])
    extends RubikCube_A_*[T] with A_*.MinimizingHeuristic[RubikCubeInstance[T]]
  {
    type Heuristic = Float
    implicit def heuristicOrdering = Ordering.Float

    implicit lazy val distanceCache = new DistanceMeasure.Cache[T]

    protected var currentHeuristic: RubikCubeInstance[T] => Heuristic = null


    def solutionLengthHeuristic = inst => listParents(inst).length

    /** Heuristic value for a state. */
    def heuristic = heuristicSingle

    def heuristicSingle: RubikCubeInstance[T] => Heuristic = inst =>
      currentHeuristic(inst) match {
        case Int.MaxValue => Int.MaxValue
        case h => h*2 + solutionLengthHeuristic(inst)
      }

//    /** A version of `heuristic` that takes into account `` */
//    def inertialHeuristic(depth: Int): RubikCubeInstance[T] => Heuristic = {
//      inst =>
//        val ps = listParents(inst)
//        val rest = if(depth > ps.length) ps else ps.take(depth)
//        (0f /: (inst +: rest)){
//          case (acc, next) => acc + heuristicSingle.apply(next)
//        } / (rest.length + 1)
//    }

    /** Extra logic ([[Decide]]) for [[searchInner]]. Intended to be overridden. */
    override protected def searchInnerExtraLogic = count =>{
      case Some((best, opn)) =>
        val newOpen = opn.extraData.get("depth to go") match {
          case None =>
            opn
              .changeExtraData(_ + ("depth to go" -> concentratedSearchDepth))
          case Some(0) =>
            opn
              .mergeToZeroPriority(opn.highestPriority._1)
              .changeExtraData(_ - "depth to go")
          case Some(i: Int) => opn
            .changeExtraData(_ + ("depth to go" -> (i-1)))
        }
        SearchInnerRecCall(best, count + 1, newOpen)
    }

    def mkHeuristic(stage: RubikCubeHeuristics.SomeTricks.Stage): RubikCubeInstance[T] => Heuristic = {
      inst =>
        val expected = stage.expectedSides[T, RubikCubeInstance[T]] apply inst
          DistanceMeasure.distance(expected.sel.values, inst) match {
            case Never => Int.MaxValue
            case Measured(i) => i
          }
    }

    /** Searches for a solution.
      * Is based on [[searchInner]],
      * @param initial The initial state.
      * @return `Some(solution)` or `None` if the solution wasn't found.
      */
    override def search(initial: RubikCubeInstance[T]) =
      Y[(Seq[RubikCubeHeuristics.SomeTricks.Stage], RubikCubeInstance[T], History[RubikCubeInstance[T]]), Result]{
        rec => {
          case (Seq(stage, rest@ _*), lastRes, hist) =>
            println("Entering stage " + (stage.expectedSides[T, RubikCubeInstance[T]] apply lastRes))
            Thread.sleep(5000)
            currentHeuristic = mkHeuristic(stage)
            val (res, newHist) = super.search(lastRes)
            res match {
              case Success(newRes) => rec((rest, newRes, hist ++ newHist))
              case fail            => (fail, hist ++ newHist)
            }
          case (Seq(), lastRes, hist) =>
            currentHeuristic = null
            Success(lastRes) -> hist
        }
      }((stages, initial, History.empty))
  }

}








object RubikCubeHeuristics{

  type Heuristic[T] = CubeWithOrientation[T] => Int


  /** From [[http://www.rubikaz.com]]. */
  object SomeTricks{

    type ExpectedSides[T] = Selection[CubeWithOrientation[T]]

    trait Stage{
      def expectedSides[T: WithSideName, C <: RubikCube[T, C]]: RubikCube[T, C] => ExpectedSides[T]
    }

    def stages: Seq[Stage] = Seq(Stage1, Stage2, Stage3)


  import SideName._

  object Stage1 extends Stage{
    def expectedSides[T: WithSideName, C <: RubikCube[T, C]] =
      _.select.cubes(Set(
          Set(Up, Front),
          Set(Up, Right),
          Set(Up, Back),
          Set(Up, Left)
        ))
  }

    object Stage2 extends Stage{
      def expectedSides[T: WithSideName, C <: RubikCube[T, C]] = _.select.cubesForming(Up)
    }

//    object Stage3 extends Stage{
//      def expectedSides[T: WithSideName, C <: RubikCube[T, C]] = i =>
//        i.select.cubesForming(Up) ++
//        i.select.cubesForming(Left, Front)
//          .filterNot(_ contains SideName.Down )
//    }

    object Stage3 extends Stage{
      def expectedSides[T: WithSideName, C <: RubikCube[T, C]] = i =>
        i.select.cubesForming(Up) ++
        i.select.cubesForming(Left, Right, Front, Back)
            //        i.select.cubesForming(Left, Front)
          .filterNot(_ contains SideName.Down )
    }

    object Stage4 extends Stage{
      def expectedSides[T: WithSideName, C <: RubikCube[T, C]] = i =>
        i.select.cubesForming(Up) ++
        i.select.cubesForming(Left, Right, Front, Back)
          .filterNot(_ contains SideName.Down )
    }

    object NewStages{
      def apply() = Seq(Stage1, Stage2, Stage3, Stage4, Stage5)

      def Stage1 = SomeTricks.Stage1
      def Stage2 = SomeTricks.Stage2

      object Stage3 extends Stage{
        def expectedSides[T: WithSideName, C <: RubikCube[T, C]] =
          i =>
            i.select.cubesForming(Up) ++
            i.select.cubes(Set(
              Set(Down, Front),
              Set(Down, Right),
              Set(Down, Back),
              Set(Down, Left)
            ))
      }
      object Stage4 extends Stage{
        def expectedSides[T: WithSideName, C <: RubikCube[T, C]] =
          i =>
            i.select.cubesForming(Up) ++
            i.select.cubesForming(Down)
      }

      object Stage5 extends Stage{
        def expectedSides[T: WithSideName, C <: RubikCube[T, C]] = i =>
          i.select.cubesForming(Up, Down, Front, Back)
      }
    }

    def selectCubes[T: WithSideName, C <: RubikCube[T, C]](stage: Stage)
                                                          (rubik: RubikCube[T, C]): Iterable[CubeWithOrientation[T]] =
      stage.expectedSides[T, C].apply(rubik).sel.values

  }



  /** Measures the "distance" between subcubes current positions and their goals. */
  object DistanceMeasure{
    var DEBUG = false

    case class CacheKey[T: WithSideName](cwo: CubeWithOrientation[T]){
      def from: Seq[SideName] = cwo.o.toSeq.filter(_ != null)
      def to  : Seq[SideName] = cwo.cube.labels.map(_.side)
    }

    sealed trait Measure{
      def toOption: Option[Int]

      def map(f: Int => Int): Measure
      def flatMap(f: Int => Measure): Measure

      def +(that: Measure): Measure = for{ x <- this; y <- that } yield x + y
    }
    case class Measured(rotations: Int) extends Measure{
      assert(rotations >= 0)

      def toOption = Some(rotations)
      def map(f: Int => Int) = Measured(f(rotations))
      def flatMap(f: Int => Measure) = f(rotations)
    }
    case object Never extends Measure{
      def toOption = None
      def map(f: Int => Int) = Never
      def flatMap(f: Int => Measure) = Never
    }

    case object Zero extends Measure{
      def toOption = None
      def map(f: Int => Int) = Measured(f(0))
      def flatMap(f: Int => Measure) = f(0)
    }

    case class MeasureFunc[T](func: (RubikCubeInstance[T], CacheKey[T]) => Measure)
    def measure[T: MeasureFunc](inst: RubikCubeInstance[T], k: CacheKey[T]): Measure =
      implicitly[MeasureFunc[T]].func(inst, k)

    class Cache[T: MeasureFunc]{
      protected val cache: mutable.HashMap[CacheKey[T], Measure] = mutable.HashMap.empty
      def get(k: CacheKey[T], inst: RubikCubeInstance[T]): Measure = cache.getOrElseUpdate(k, measure(inst, k))
    }

    def distance[T: Cache: WithSideName](cwo: CubeWithOrientation[T], inst: RubikCubeInstance[T]): Measure =
      implicitly[Cache[T]].get(CacheKey(cwo), inst)

    def distance[T: Cache: WithSideName](cwos: Iterable[CubeWithOrientation[T]], inst: RubikCubeInstance[T]): Measure =
      ((Zero: Measure) /: cwos.map(distance(_: CubeWithOrientation[T], inst))) { case (acc, r) => acc + r } match {
        case Zero => Never
        case m    => m
      }

    /** Foreach possible rotation sequence*, find which ones lead to goal position and select the shortest one.
     *  * A 'rotation sequence' here is the sequence of rotated sides (can be any number of side's rotations:, 0-3).
     */
    protected def calcDistanceToGoal[T: WithSideName](inst: RubikCubeInstance[T], k: CacheKey[T]): Measure = {
      val routes: Seq[Int] = for{
        rotSeq  <- SideName.values.toList.permutations.toStream //k.from.permutations.toStream
        measure <- sideRotatingDistance(inst, k.cwo.cube.cubeId, k.to, rotSeq, 0).toOption
        } yield measure

      if (DEBUG) {
        println("measures for " + k)
        println(routes.toList)
        println()
      }

      if(routes.isEmpty) sys.error("no route found for " + k.cwo)
      else Measured(routes.min)
    }


    protected def sideRotatingDistance[T: WithSideName](inst: RubikCubeInstance[T],
                                                        cId: CubeId,
                                                        cubeSideSeq: Seq[SideName],
                                                        sideRotSeq: Seq[SideName],
                                                        rotAcc: Int): Measure =
      if(cId.size == 1) Measured(0)
      else cubeSideSeq -> sideRotSeq match {
        case (Seq(cSide, cTail@ _*), Seq(rSide, rTail@ _*)) =>
          val Some(cwo) = inst.findCubeById(cId)
          val from = cwo.selectOrientationBySide(cSide)
          val dist = sideRotatingPartialDistance(from, cSide, rSide)

          if (DEBUG) println(s"sideRotatingDistance: from=$from, to=$cSide, rSide=$rSide, dist=$dist")

          dist match {
            case Measured(n) =>
              val newInst = (inst /: (1 to n)){ case (i, _) => i.rotate(rSide) }
              sideRotatingDistance(newInst, cId, cTail, rTail, rotAcc + n)
            case Never => Never
          }
        case (Seq(), _) => Measured(rotAcc)
      }




    protected def sideRotatingPartialDistance[T: WithSideName](from: SideName,
                                                               to: SideName,
                                                               rotating: SideName): Measure =
      Y[(SideName, Int), Measure](
        rec => {
          case (prev, c) =>
            val next = Rotation.next.byName(rotating)(prev)
            if      (prev == to)   Measured(c)
            else if (prev == next) Never
            else if (c > 3)        Never
            else                   rec(next, c+1)
          }
      )(from -> 0)


    def defaultMeasure[T: WithSideName]: MeasureFunc[T] = MeasureFunc(calcDistanceToGoal[T])

  }

  trait Selection[A]{
    def sel: Map[CubeId, A]

    def mapValues[R](f: A => R): Map[CubeId, R] = sel.mapValues(f)
    def map[R](f: (CubeId, A) => R): Iterable[R] = sel.map(f.tupled)
    def fold[R](r0: R)(f: (R, (CubeId, A)) => R): R = sel.foldLeft(r0)(f)
  }

  case class CubesSelection[T: WithSideName](sel: Map[CubeId, CubeWithOrientation[T]])
    extends Selection[CubeWithOrientation[T]]
  {
    def filter(f: CubeId => Boolean) = copy(sel.filterKeys(f))
    def filterNot(f: CubeId => Boolean) = copy(sel.filterKeys(not compose f))

    def ++ (that: CubesSelection[T]) = copy(this.sel ++ that.sel)
  }


  implicit class RubikHeuristicsHelper[T: WithSideName, C <: RubikCube[T, C]](r: RubikCube[T, C]){

    object select{

      def positions(ids: Set[CubeId]): CubesSelection[T] = CubesSelection(r.cubesPositions.filterKeys(ids.contains))
      def positions(ids: (Int, Int, Int)*): CubesSelection[T] = positions(ids.toSet.map(cubeAt))
      def positions(ids: Map[SideName, Set[(Int, Int)]]): CubesSelection[T] =
        positions( ids.flatMap{ case (id, ps) => ps.map(sideCubes(id)) }.toSet )

      def cubes(ids: Set[CubeId]): CubesSelection[T] =
        CubesSelection(r.cubesPositions.filter(ids contains _._2.cube.cubeId))

      def cubesForming(sideNames: SideName*): CubesSelection[T] = cubes(sideNames.toSet.map(sideCubes).flatMap(_.values))

    }
  }

  object DistanceMeasureOLD{

    /** TODO: it seems to include also the the 'orientation distance' */
    def moveDistance[T: WithSideName]: Heuristic[T] = {
      case CubeWithOrientation(c, o) => {
        val abs = (0 /: c.labels.zip(o.toSeq)){
          case (acc, (x, side)) => acc + (if (x.side == side) 0 else 1)
        }
        val rel = (c.labels.map(_.side).toSet -- o.toSeq.toSet).size

//        println(s"abs=$abs, rel=$rel")
        abs + rel
      }
    }

  }


}
