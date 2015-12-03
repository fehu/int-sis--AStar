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

  class WithTricks[T: WithSideName](val stage: RubikCubeHeuristics.SomeTricks.Stage,
                                    implicit val measureFunc: MeasureFunc[T])
    extends RubikCube_A_*[T] with A_*.MinimizingHeuristic[RubikCubeInstance[T]]
  {
    type Heuristic = Int
    implicit def heuristicOrdering = Ordering.Int

    implicit lazy val distanceCache = new DistanceMeasure.Cache[T]


    /** Heuristic value for a state. */
    lazy val heuristic: RubikCubeInstance[T] => Heuristic = _.cubeById.values |> {
      cMap =>
        DistanceMeasure.distance(cMap) match {
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

    case class CubesSideSelector(side: SideName)

    type SideColor = SideName
    type ExpectedSides = Set[Either[(SideName, CubeId), CubesSideSelector]]

    trait Stage{
      def expectedSides: ExpectedSides
      def avoidRotating: Set[SideName]
    }


    import SideName._
/*
    TODO: stages should filter the cubes by their COLOR, not position
 */
    object Stage1 extends Stage{
      lazy val expectedSides: ExpectedSides = Set(
        Up    -> Set(Up, Front),
        Front -> Set(Up, Front),
        Up    -> Set(Up, Right),
        Right -> Set(Up, Right),
        Up    -> Set(Up, Left),
        Up    -> Set(Up, Back)
      ).map(scala.Left(_))
      def avoidRotating = Set()
    }


    object Stage2 extends Stage{
      def expectedSides: ExpectedSides =  Stage1.expectedSides + scala.Right(CubesSideSelector(Up)) ++ Set(
        Front -> Set(Front, Left, Up),
        Front -> Set(Front, Right, Up),
        Right -> Set(Front, Right, Up),
        Right -> Set(Back , Right, Up)
      ).map(scala.Left(_))
      def avoidRotating = Set()
    }

    def filterFunc[T: WithSideName](stage: Stage)(cwo: CubeWithOrientation[T]): Boolean = {
      val considered = stage.expectedSides
        .flatMap(
          _ .right.map(s => sideCubes(s.side).map(s.side -> _._2).toSet)
            .left.map(Set(_))
            .merge
        )
        .map(_.swap)
        .toMap

        considered contains cwo.cube.cubeId
    }

    def filtering[T: WithSideName](stage: Stage, h: Heuristic[T]): Heuristic[T] =
      cwo => if(filterFunc(stage)(cwo)) h(cwo) else 0
  }



  /** Measures the "distance" between subcubes current positions and their goals. */
  object DistanceMeasure{
    case class CacheKey[T: WithSideName](cube: CubeWithOrientation[T], square: SideName, rotating: SideName){
      def from: SideName = cube.selectSide(square).side
      def to  : SideName = cube.selectOrientation(square)
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

//  case class CorrectOrientation protected (n: Int, of: Int)
//
//  object CorrectOrientation{
//
//    def apply[T: WithSideName](c: CubeWithOrientation[T]): CorrectOrientation =
//      CorrectOrientation(
//        c.cube.labels.map(_.side).zip(c.o.toSeq).count{ case (x,y) => x == y },
//        c.cube.labels.size
//      )
//
//    def apply[T: WithSideName](c: Iterable[CubeWithOrientation[T]]): Iterable[CorrectOrientation] = c.map(apply[T])
//
//  }

/*
  implicit class RubikHeuristicsHelper[T: WithSideName, C <: RubikCube[T, C]](r: RubikCube[T, C]){

    object select{

      def cubes(ids: Set[CubeId]): CubesSelection = CubesSelection(r.cubeById.filterKeys(ids.contains))
      def cubes(ids: (Int, Int, Int)*): CubesSelection = cubes(ids.toSet.map(cubeAt))
      def cubes(ids: Map[SideName, Set[(Int, Int)]]): CubesSelection =
        cubes( ids.flatMap{ case (id, ps) => ps.map(sideCubes(id)) }.toSet )

      def cubesForming(sideNames: SideName*): CubesSelection = cubes(sideNames.toSet.map(sideCubes).flatMap(_.values))

      def sides(sideNames: SideName*): CubesSideSelection = {
        val cs = cubesForming(sideNames: _*)
        CubesSideSelection(sideNames.flatMap(cs.fromSide _ andThen (_.sel)).toMap)
      }


      trait Selection[A]{
        def sel: Map[CubeId, A]

        def mapValues[R](f: A => R): Map[CubeId, R] = sel.mapValues(f)
        def map[R](f: (CubeId, A) => R): Iterable[R] = sel.map(f.tupled)
        def fold[R](r0: R)(f: (R, (CubeId, A)) => R): R = sel.foldLeft(r0)(f)
      }

      case class CubeSide(side: SideName, current: SideName){
        def correctlySet = side == current
      }

      case class CubesSelection(sel: Map[CubeId, CubeWithOrientation[T]])
        extends Selection[CubeWithOrientation[T]]
      {
        def fromSide(sideName: SideName): CubesSideSelection = CubesSideSelection(
          sel
            .withFilter(_._2.o.toSeq contains sideName)
            .map{ case (id, cwo) => id -> CubeSide(sideName, cwo.colorFrom(sideName).get.side) }
        )
      }

      case class CubesSideSelection(sel: Map[CubeId, CubeSide]) extends Selection[CubeSide]
    }

  }
*/

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