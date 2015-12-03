package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.util._

/** Immutable Rubik's Cube */
case class RubikCubeInstance[T] (cubesPositions: Map[CubeId, CubeWithOrientation[T]],
                                 parent: Option[RubikCubeInstance[T]],
                                 description: RubikCube.Description )
  extends RubikCube[T, RubikCubeInstance[T]]
{
  type ThisType = RubikCubeInstance[T]

  def rawSides: Map[SideName, Map[(Int, Int), CubeWithOrientation[T]]] =
    SideName.values.toSeq.zipMap{ sideName => RubikCube.sideCubes(sideName).mapValues(cubesPositions) }.toMap


  def rotate(side: SideName): RubikCubeInstance[T] = {
    val upd = rotateUpdate(side).map{ case Update(c, o, pos) => pos -> CubeWithOrientation(c, o) }
    RubikCubeInstance(cubesPositions ++ upd, Some(this), RubikCube.Rotation(RotationAngle.Rot90, side))
  }

  def snapshot = this

  override def equals(obj: scala.Any) = canEqual(obj) && (obj match{
    case that: RubikCubeInstance[T] => this.cubesPositions == that.cubesPositions
  })
}


object RubikCubeInstance{

  class MutableContainer[T](protected var instance: RubikCubeInstance[T]) extends RubikCube[T, MutableContainer[T]]
  {
    type ThisType = MutableContainer[T]

    def set(i: RubikCubeInstance[T]) = instance = i
    def get = instance

    /** rotate a side 90 degrees clockwise */
    def rotate(sideName: SideName) = {
      instance = instance.rotate(sideName)
      this
    }

    def cubesPositions = instance.cubesPositions

    def snapshot = get
  }

}