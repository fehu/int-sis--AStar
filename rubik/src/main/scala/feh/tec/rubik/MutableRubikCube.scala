package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.util._

import scala.collection.immutable.Iterable
import scala.collection.mutable

/** Mutable Rubik's Cube */
class MutableRubikCube[T: WithSideName](initialCubes: Set[Cube[T]]) extends RubikCube[T, MutableRubikCube[T]]
{

  type ThisType = MutableRubikCube[T]

  def cubesPositions = cubesHeap.toMap

  /** rotate a side 90 degrees clockwise */
  def rotate(sideName: SideName) = { bulkUpdate(rotateUpdate(sideName)); this }
  
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
  
  
  protected lazy val corners = mutable.HashMap(
    RubikSubCubes.corners.map{
      case ((s1, _), (s2, _), (s3, _)) =>
        val id = Set(s1, s2, s3)
        val c = initialCubes
          .collectFirst{ case c: Corner[T] if id == cubeId(c) => c }
          .get

        id -> CubeWithOrientation(c, CubeOrientation(c.label1.side, c.label2.side, c.label3.side))
    }: _*
  )

  protected lazy val middles = mutable.HashMap(
    RubikSubCubes.middles.map{
      case ((s1, _), (s2, _)) =>
        val id = Set(s1, s2)
        val c = initialCubes
          .collectFirst{ case m: Middle[T] if id == cubeId(m) => m }
          .get

        id -> CubeWithOrientation(c, CubeOrientation(c.label1.side, c.label2.side, null))
    }: _*
  )

  protected lazy val centers = mutable.HashMap(
    SideName.values.toSeq.map{
      side => 
        val c = initialCubes
          .collectFirst{ case c@Center(l) if l.side == side => c }
          .get
        
        Set(side) -> CubeWithOrientation(c, CubeOrientation(side, null, null))
    }: _*)

  def snapshot = RubikCubeInstance[T](cubes.mapKeys(RubikCube.cubeAt), None, RubikCube.NoDescription)
}
