package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.util._

/** Immutable Rubik's Cube */
case class RubikCubeInstance[T] (cubeById: Map[CubeId, CubeWithOrientation[T]],
                                 parent: Option[RubikCubeInstance[T]],
                                 description: String )
  extends RubikCube[T]
{
  type ThisType = RubikCubeInstance[T]

  def rawSides: Map[SideName, Map[(Int, Int), CubeWithOrientation[T]]] =
    SideName.values.toSeq.zipMap{ sideName => RubikCube.sideCubes(sideName).mapValues(cubeById) }.toMap


  def rotate(side: SideName): RubikCubeInstance[T] = {
    val upd = rotateUpdate(side).map{ case Update(c, o, pos) => pos -> (c, o) }
    RubikCubeInstance(cubeById ++ upd, Some(this), "rotate " + side.toString + " 90° clockwise")
  }

  def snapshot = this

  override def equals(obj: scala.Any) = canEqual(obj) && (obj match{
    case that: RubikCubeInstance[T] => this.cubeById == that.cubeById
  })
}


object RubikCubeInstance{

  class MutableContainer[T](protected var instance: RubikCubeInstance[T]) extends RubikCube[T]
  {
    type ThisType = MutableContainer[T]

    def set(i: RubikCubeInstance[T]) = instance = i
    def get = instance

    /** rotate a side 90 degrees clockwise */
    def rotate(sideName: SideName) = {
      instance = instance.rotate(sideName)
      this
    }

    def cubeById = instance.cubeById

    def snapshot = get
  }

}