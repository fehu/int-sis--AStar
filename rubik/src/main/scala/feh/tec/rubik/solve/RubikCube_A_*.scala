package feh.tec.rubik.solve

import feh.tec.astar.A_*
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.solve.RubikCubeHeuristics.DistanceMeasure.{MeasureFunc, Measured, Never}
import feh.tec.rubik.{RubikCube, RubikCubeInstance}
import feh.tec.rubik.solve.RubikCubeHeuristics.{DistanceMeasure, DistanceMeasureOLD, SomeTricks}
import feh.util._

import scala.collection.mutable

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

  class WithTricksStage[T: WithSideName](val stage: RubikCubeHeuristics.SomeTricks.Stage,
                                         implicit val measureFunc: MeasureFunc[T])
    extends RubikCube_A_*[T] with A_*.MinimizingHeuristic[RubikCubeInstance[T]]
  {
    type Heuristic = Int
    implicit def heuristicOrdering = Ordering.Int

    implicit lazy val distanceCache = new DistanceMeasure.Cache[T]


    /** Heuristic value for a state. */
    lazy val heuristic: RubikCubeInstance[T] => Heuristic = stage.expectedSides[T, RubikCubeInstance[T]] andThen {
      expected =>
        DistanceMeasure.distance(expected.sel.values) match {
          case Never       => Int.MaxValue
          case Measured(i) => i
        }
    }
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
    case class CacheKey[T: WithSideName](cube: CubeWithOrientation[T], square: SideName, rotating: SideName){
      def from: SideName = cube.selectOrientation(square)
      def to  : SideName = cube.selectSide(square).side
    }

    sealed trait Measure { def toOption: Option[Int] }
    case class Measured(rotations: Int) extends Measure{ def toOption = Some(rotations) }
    case object Never                   extends Measure{ def toOption = None }

    case class MeasureFunc[T](func: CacheKey[T] => Measure)
    def measure[T: MeasureFunc](k: CacheKey[T]): Measure = implicitly[MeasureFunc[T]].func(k)

    class Cache[T: MeasureFunc]{
      protected val cache: mutable.HashMap[CacheKey[T], Measure] = mutable.HashMap.empty
      def get(k: CacheKey[T]): Measure = cache.getOrElseUpdate(k, measure(k))
    }

    def distance[T: Cache: WithSideName](cwo: CubeWithOrientation[T],
                                         square: SideName,
                                         rotating: SideName): Measure =
      implicitly[Cache[T]].get(CacheKey(cwo, square, rotating))


    def distance[T: Cache: WithSideName](cwo: CubeWithOrientation[T]): Measure = {
      val dists = for {
        (c, o)  <- cwo.coSeq
        rotSide <- cwo.o.toSeq

        if rotSide != null
        key  = CacheKey(cwo, o, rotSide)
        dist = implicitly[Cache[T]] get key
      } yield dist.toOption getOrElse 0

      Measured(dists.sum)
    }

    def distance[T: Cache: WithSideName](cwos: Iterable[CubeWithOrientation[T]]): Measure =
      ((Measured(0): Measure) /: cwos.map(distance(_: CubeWithOrientation[T]))) {
        case (Measured(acc), Measured(r)) => Measured(acc + r)
        case (Never, _) => Never
        case (_, Never) => Never
      }

    protected def calcDistanceToGoal[T: WithSideName](k: CacheKey[T]) =
      if (k.from == k.to) Measured(0)
      else Y[(SideName, Int), Measure](
        rec => {
          case (prev, c) =>
            val next = Rotation.next.byName(k.rotating)(prev)
            if      (prev == next) Never
            else if (c > 4)        Never
            else if (prev == k.to) Measured(c)
            else                   rec(next, c+1)
          }
      )(k.from -> 1)

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