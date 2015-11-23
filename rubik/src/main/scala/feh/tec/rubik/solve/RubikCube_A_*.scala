package feh.tec.rubik.solve

import feh.tec.astar.A_*
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.{RubikCube, RubikCubeInstance}

/** Implements A* methods related to [[RubikCubeInstance]]s.
  */
trait RubikCube_A_*[T] extends A_*[RubikCubeInstance[T]]{
  /** Lists the next possible states. */
  def transformations = c => SideName.values.toSeq.map(c.rotate)

  /** Is the given state a solution? */
  def isSolution = _.cubeById.forall{ case (_, (c, o)) => c.labels == o.toSeq }

  /** List state's parents. */
  protected def listParents: RubikCubeInstance[T] => Seq[RubikCubeInstance[T]] = c => c.parent match {
    case Some(p) => p +: listParents(p)
    case _       => Stream.empty
  }

  /** A human readable description for a state. */
  def description = _.description
}

object RubikCubeHeuristics{

  case class CorrectOrientation protected (n: Int, of: Int)
  
  object CorrectOrientation{
    
    def apply[T: WithSideName](c: CubeWithOrientation[T]): CorrectOrientation =
      CorrectOrientation(
        c._1.labels.map(_.side).zip(c._2.toSeq).count{ case (x,y) => x == y },
        c._1.labels.size
      )

    def apply[T: WithSideName](c: Iterable[CubeWithOrientation[T]]): Iterable[CorrectOrientation] = c.map(apply[T])

  }

  implicit class RubikHeuristicsHelper[T: WithSideName](r: RubikCube[T]){
    
    object select{
      
      def cubes(ids: Set[CubeId]): CubesSelection = CubesSelection(r.cubeById.filterKeys(ids.contains))
      def cubes(ids: (Int, Int, Int)*): CubesSelection = cubes(ids.toSet.map(cubeAt))
      def cubes(ids: Map[SideName, Set[(Int, Int)]]): CubesSelection =
        cubes( ids.flatMap{ case (id, ps) => ps.map(sideCubes(id)) }.toSet )
//      def cubes(sideNames: SideName*): CubesSelection = cubes(sideNames.toSet.map(sideCubes).flatMap(_.values))
//
//      def sides(sideNames: SideName*): CubesSideSelection = {
//        val cs = cubes(sideNames: _*)
//        CubesSideSelection(sideNames.flatMap(cs.fromSide _ andThen (_.sel)).toMap)
//      }
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
          .withFilter(_._2._2.toSeq contains sideName)
          .map{ case (id, (c, o)) => id -> CubeSide(sideName, c.labels(o.toSeq.indexOf(sideName)).side) }
      )
    }
    case class CubesSideSelection(sel: Map[CubeId, CubeSide]) extends Selection[CubeSide]
  }
}