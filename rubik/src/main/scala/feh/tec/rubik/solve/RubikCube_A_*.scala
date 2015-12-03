package feh.tec.rubik.solve

import feh.tec.astar.{HistoryRecord, History, A_*}
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.solve.RubikCubeHeuristics.DistanceMeasure.{MeasureFunc, Measured, Never}
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
  def isSolution = heuristic andThen (_ == 0)
//    _.cubeById.forall{ case (_, CubeWithOrientation(c, o)) => c.labels == o.toSeq }

  /** List state's parents. */
  def listParents: RubikCubeInstance[T] => Seq[RubikCubeInstance[T]] = c => c.parent match {
    case Some(p) => p +: listParents(p)
    case _       => Stream.empty
  }

  /** A human readable description for a state. */
  def description = _.description.toString

  def fullDescription: RubikCubeInstance[T] => RubikCube.Description = _.description

}




object RubikCube_A_*{

  /**
    */
  class WithTricksStages[T: WithSideName](val stages: Seq[RubikCubeHeuristics.SomeTricks.Stage],
                                          implicit val measureFunc: MeasureFunc[T])
    extends RubikCube_A_*[T] with A_*.MinimizingHeuristic[RubikCubeInstance[T]]
  {
    type Heuristic = Int
    implicit def heuristicOrdering = Ordering.Int

    implicit lazy val distanceCache = new DistanceMeasure.Cache[T]

    protected var currentHeuristic: RubikCubeInstance[T] => Heuristic = null

    /** Heuristic value for a state. */
    def heuristic: RubikCubeInstance[T] => Heuristic = currentHeuristic

    def mkHeuristic(stage: RubikCubeHeuristics.SomeTricks.Stage): RubikCubeInstance[T] => Heuristic =
      stage.expectedSides[T, RubikCubeInstance[T]] andThen {
        expected =>
          DistanceMeasure.distance(expected.sel.values) match {
            case Never       => Int.MaxValue
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

    def stages: Seq[Stage] = Seq(Stage1) // , Stage2)


    import SideName._
/*
    TODO: stages should filter the cubes by their COLOR, not position
 */
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


    def selectCubes[T: WithSideName, C <: RubikCube[T, C]](stage: Stage)
                                                          (rubik: RubikCube[T, C]): Iterable[CubeWithOrientation[T]] =
      stage.expectedSides[T, C].apply(rubik).sel.values

  }



  /** Measures the "distance" between subcubes current positions and their goals. */
  object DistanceMeasure{
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

    case class MeasureFunc[T](func: CacheKey[T] => Measure)
    def measure[T: MeasureFunc](k: CacheKey[T]): Measure = implicitly[MeasureFunc[T]].func(k)

    class Cache[T: MeasureFunc]{
      protected val cache: mutable.HashMap[CacheKey[T], Measure] = mutable.HashMap.empty
      def get(k: CacheKey[T]): Measure = cache.getOrElseUpdate(k, measure(k))
    }

    def distance[T: Cache: WithSideName](cwo: CubeWithOrientation[T]): Measure = implicitly[Cache[T]].get(CacheKey(cwo))

    def distance[T: Cache: WithSideName](cwos: Iterable[CubeWithOrientation[T]]): Measure =
      ((Measured(0): Measure) /: cwos.map(distance(_: CubeWithOrientation[T]))) {
        case (Measured(acc), Measured(r)) => Measured(acc + r)
        case (Never, _) => Never
        case (_, Never) => Never
      }

    /** Foreach possible rotation sequence*, find which ones lead to goal position and select the shortest one.
     *  * A 'rotation sequence' here is the sequence of rotated sides (can be any number of side's rotations:, 0-3).
     */
    protected def calcDistanceToGoal[T: WithSideName](k: CacheKey[T]): Measure = {
      val sidesSeq = k.from zip k.to

      val yieldRotations = Y[Seq[(SideName, (SideName, SideName))], Measure](
        rec => {
          case Seq((rotSide, (from, to)), tail@ _*) => sideRotatingPartialDistance(from, to, rotSide) + rec(tail)
          case Nil => Measured(0)
        }
      )

      val routes: Seq[Int] = for{
        rotSeq  <- k.from.permutations.toStream
        measure <- yieldRotations(rotSeq zip sidesSeq).toOption
        } yield measure

      if(routes.isEmpty) Never
      else Measured(routes.min)
    }

    protected def sideRotatingPartialDistance[T: WithSideName](from: SideName,
                                                               to: SideName,
                                                               rotating: SideName): Measure =
      if (from == to) Measured(0)
      else Y[(SideName, Int), Measure](
        rec => {
          case (prev, c) =>
            val next = Rotation.next.byName(rotating)(prev)
            if      (prev == next) Never
            else if (c > 3)        Never
            else if (prev == to) Measured(c)
            else                   rec(next, c+1)
          }
      )(from -> 1)

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


  implicit class RubikHeuristicsHelper[T: WithSideName, C <: RubikCube[T, C]](r: RubikCube[T, C]){

    object select{

      def positions(ids: Set[CubeId]): CubesSelection[T] = CubesSelection(r.cubesPositions.filterKeys(ids.contains))
      def positions(ids: (Int, Int, Int)*): CubesSelection[T] = cubes(ids.toSet.map(cubeAt))
      def positions(ids: Map[SideName, Set[(Int, Int)]]): CubesSelection[T] =
        cubes( ids.flatMap{ case (id, ps) => ps.map(sideCubes(id)) }.toSet )

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