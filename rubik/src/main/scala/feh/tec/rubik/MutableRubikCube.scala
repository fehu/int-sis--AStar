package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl.Utils.Angle0
import feh.util._

import scala.collection.immutable.Iterable
import scala.collection.mutable

/** Mutable Rubik's Cube */
class MutableRubikCube[T: WithSideName](initialCubes: Set[Cube[T]]) extends RubikCube[T]
{
  def cubes: Map[(Int, Int, Int), CubeWithOrientation[T]] = RubikCube.cubeAt.mapValues(cubesHeap)

  def sides(sideName: SideName): Map[(Int, Int), CubeWithOrientation[T]] =
    RubikCube.sideCubes(sideName).mapValues(cubesHeap)
  

  /** rotate a side 90 degrees clockwise */
  def rotate(sideName: SideName): Unit = bulkUpdate{
    for ( (pos, (c, o)) <- sides(sideName) )
      yield Update(c, o.rotate(sideName), MutableRubikCube.rotationPosChange(sideName, pos))
  }
  
  case class Update(put: Cube[T], o: CubeOrientation, at: CubeId)
  
  def bulkUpdate(upds: Iterable[Update]): Unit = {
    upds.foreach{ case Update(c, o, at) => putInCube(at, c, o) }
  }
  
  private def putInCube(at: CubeId, c: Cube[T], o: CubeOrientation) = at.size match {
    case 1 => centers += at -> (c.asInstanceOf[Center[T]], o)
    case 2 => middles += at -> (c.asInstanceOf[Middle[T]], o)
    case 3 => corners += at -> (c.asInstanceOf[Corner[T]], o)
  }
  

  private def cubeId(c: Cube[T]) =  c.labels.map(_.side).toSet

  def cubesHeap = corners ++ middles ++ centers
  
  
  protected def defaultOrientation = CubeOrientation(Angle0, Angle0, Angle0)
  
  protected lazy val corners = mutable.HashMap(
    RubikSubCubes.corners.map{
      case ((s1, _), (s2, _), (s3, _)) =>
        val id = Set(s1, s2, s3)
        val c = initialCubes
          .collectFirst{ case c: Corner[T] if id == cubeId(c) => c }
          .get
        val o = defaultOrientation

        id -> (c, o)
    }: _*
  )

  protected lazy val middles = mutable.HashMap(
    RubikSubCubes.middles.map{
      case ((s1, _), (s2, _)) =>
        val id = Set(s1, s2)
        val c = initialCubes
          .collectFirst{ case m: Middle[T] if id == cubeId(m) => m }
          .get

        id -> (c, defaultOrientation)
    }: _*
  )

  protected lazy val centers = mutable.HashMap(
    SideName.values.toSeq.map{
      side => 
        val c = initialCubes
          .collectFirst{ case c@Center(l) if l.side == side => c }
          .get
        
        Set(side) -> (c, defaultOrientation)
    }: _*)

  def snapshot = RubikCubeInstance[T](cubes.mapKeys(RubikCube.cubeAt))
}

object MutableRubikCube{
  
  def rotationPosChange(side: SideName, pos: (Int, Int)): CubeId =
    sideCubes(side)(rotationIntPosChange(pos))

  def rotationPosChange(side: SideName, pos: CubeId): CubeId =
    rotationPosChange(side, sidePositions(side)(pos))
  
  lazy val rotationIntPosChange = Map(
    (0, 2) -> (2, 2),
    (1, 2) -> (2, 1),
    (2, 2) -> (2, 0),
    (2, 1) -> (1, 0),
    (2, 0) -> (0, 0),
    (1, 0) -> (0, 1),
    (0, 0) -> (0, 2),
    (0, 1) -> (1, 2),
    (1, 1) -> (1, 1)
  )


}
