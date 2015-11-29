package feh.tec.rubik.solve

import feh.tec.astar.A_*
import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.{RubikCube, RubikCubeInstance}

/** Implements A* methods related to [[RubikCubeInstance]]s.
  */
trait RubikCube_A_*[T] extends A_*[RubikCubeInstance[T]]{
  /** Lists the next possible states. */
  def transformations = c => SideName.values.toSeq.map(c.rotate)

  /** Is the given state a solution? */
  def isSolution = _.cubeById.forall{ case (_, CubeWithOrientation(c, o)) => c.labels == o.toSeq }

  /** List state's parents. */
  def listParents: RubikCubeInstance[T] => Seq[RubikCubeInstance[T]] = c => c.parent match {
    case Some(p) => p +: listParents(p)
    case _       => Stream.empty
  }

  /** A human readable description for a state. */
  def description = _.description
}




object RubikCube_A_*{

  class WithTricks[T: WithSideName](val stage: RubikCubeHeuristics.SomeTricks.Stage)
    extends RubikCube_A_*[T] with A_*.MaximizingHeuristic[RubikCubeInstance[T]]
  {
    type Heuristic = Int
    implicit def heuristicOrdering = Ordering.Int

    /** Heuristic value for a state. */
    lazy val heuristic: RubikCubeInstance[T] => Heuristic = _
      .cubeById.values
      .map( RubikCubeHeuristics.SomeTricks.chain(stage, RubikCubeHeuristics.DistanceMeasure.moveDistance) )
      .sum
  }

}








object RubikCubeHeuristics{

  case class CorrectOrientation protected (n: Int, of: Int)
  
  object CorrectOrientation{
    
    def apply[T: WithSideName](c: CubeWithOrientation[T]): CorrectOrientation =
      CorrectOrientation(
        c.cube.labels.map(_.side).zip(c.o.toSeq).count{ case (x,y) => x == y },
        c.cube.labels.size
      )

    def apply[T: WithSideName](c: Iterable[CubeWithOrientation[T]]): Iterable[CorrectOrientation] = c.map(apply[T])

  }

  implicit class RubikHeuristicsHelper[T: WithSideName](r: RubikCube[T]){
    
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
    
    def heuristic[T: WithSideName](stage: Stage): Heuristic[T] = {
      val expected = stage.expectedSides
        .flatMap(
          _ .right.map(s => sideCubes(s.side).map(s.side -> _._2).toSet)
            .left.map(Set(_))
            .merge
        )
        .map(_.swap)
        .toMap
          
      cwo => 
        expected.get(cwo.cube.cubeId)
          .map{ exp => if (cwo.colorFrom(exp) contains exp) 1 else -1 }
          .getOrElse(0)
    } 

    def chain[T: WithSideName](stage: Stage, next: Heuristic[T]): Heuristic[T] =
      cwo => heuristic(stage) apply cwo match {
        case 0  => 0
        case 1  => 1
        case -1 => - next(cwo)
      }
  }



  /** Measures the "distance" between subcubes and their goals. */
  object DistanceMeasure{

    /** TODO: it seems to include also the the 'orientation distance' */
    def moveDistance[T: WithSideName]: Heuristic[T] = {
      case CubeWithOrientation(c, o) => (c.labels.map(_.side).toSet -- o.toSeq.toSet).size
    }

  }


}