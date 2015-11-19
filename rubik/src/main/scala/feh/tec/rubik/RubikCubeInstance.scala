package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.util._

/** Immutable Rubik's Cube */
case class RubikCubeInstance[T] (cubeById: Map[CubeId, CubeWithOrientation[T]]) extends RubikCube[T]
{

  def cubes = cubeById.mapKeys(RubikCube.cubePosition)

  def rawSides: Map[SideName, Map[(Int, Int), CubeWithOrientation[T]]] =
    SideName.values.toSeq.zipMap{ sideName => RubikCube.sideCubes(sideName).mapValues(cubeById) }.toMap




//  lazy val sides: Map[SideName, Side[T]] = rawSides.mapValues{
//    sideCubes =>
//      Side(
//        sideCubes
//          .groupBy(_._1._2).toSeq.sortBy(_._1)
//            .map(_._2.toSeq.sortBy(- _._1._2).map(_._))
//      )
//  }

}


object RubikCubeInstance{

  class MutableContainer[T](protected var instance: RubikCubeInstance[T]) extends RubikCube[T]
  {
    def set(i: RubikCubeInstance[T]) = instance = i
    def get = instance

    def cubes = get.cubes
  }

//  case class CubeSide[T](cube: Cube[T], label: T)


//  lazy val side = 3

/*
  /** Rubik's Cube side.
    *
    * @param cubes list of cube's rows
    * @tparam T label
    */
  case class Side[T](cubes: List[List[CubeSide[T]]]){
    side =>

    def center = cubes(1)(1).cube.asInstanceOf[Center[T]]
    object edge{
      def top    = mkEdge(cubes.head)
      def bottom = mkEdge(cubes(2))
      def right  = mkEdge(mkRight)
      def left   = mkEdge(mkLeft)

      def byName(name: EdgeName) = name match {
        case EdgeName.Right  => right
        case EdgeName.Left   => left
        case EdgeName.Top    => top
        case EdgeName.Bottom => bottom
      }
    }

    object setEdge{
      def top(l: SideEdge[T]) = copy(l.ensuring(_.side == side).edge :: cubes.tail)
      def bottom(l: SideEdge[T]) = copy(cubes.init :+ l.ensuring(_.side == side).edge)

      def byName(name: EdgeName) = name match {
        case EdgeName.Right  => ???
        case EdgeName.Left   => ???
        case EdgeName.Top    => top _
        case EdgeName.Bottom => bottom _
      }
    }

    /** Rotate the side 90 degrees clockwise */
    def rotate90 = Side(mkLeft :: mkMiddle :: mkRight :: Nil)

    /** Rotate the side 90 degrees counter-clockwise */
    def rotate_90 = Side(mkRight :: mkMiddle :: mkLeft :: Nil)

    def rotate180 = Side(cubes.reverse map (_.reverse))

    private def mkRight  = cubes.map(_(2))
    private def mkLeft   = cubes.map(_.head)
    private def mkMiddle = cubes.map(_(1))

    private def mkEdge = SideEdge(this, _: List[CubeSide[T]])
  }
*/
//  case class SideEdge[T](side: Side[T], edge: List[CubeSide[T]]){
//    def left  = edge.head
//    def right = edge(2)
//    def middle = edge(1)
//  }
}