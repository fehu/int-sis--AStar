package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.RubikCubeInstance.RotationAngle
import feh.util._

/** Immutable Rubik's Cube */
case class RubikCubeInstance[T] (cubeById: Map[CubeId, CubeWithOrientation[T]],
                                 parent: Option[RubikCubeInstance[T]],
                                 description: RubikCubeInstance.Description )
  extends RubikCube[T]
{
  type ThisType = RubikCubeInstance[T]

  def rawSides: Map[SideName, Map[(Int, Int), CubeWithOrientation[T]]] =
    SideName.values.toSeq.zipMap{ sideName => RubikCube.sideCubes(sideName).mapValues(cubeById) }.toMap


  def rotate(side: SideName): RubikCubeInstance[T] = {
    val upd = rotateUpdate(side).map{ case Update(c, o, pos) => pos -> CubeWithOrientation(c, o) }
    RubikCubeInstance(cubeById ++ upd, Some(this), RubikCubeInstance.Rotation(RotationAngle.Rot90, side))
  }

  def snapshot = this

  override def equals(obj: scala.Any) = canEqual(obj) && (obj match{
    case that: RubikCubeInstance[T] => this.cubeById == that.cubeById
  })
}


object RubikCubeInstance{

  sealed trait Description{
    def asString: String
    override def toString: String = asString
  }

  object RotationAngle extends Enumeration{
    val Rot90, Rot180, Rot270 = Value

    def toStr(a: RotationAngle): String = a match {
      case RotationAngle.Rot90  => "90° clockwise"
      case RotationAngle.Rot180 => "180°"
      case RotationAngle.Rot270 => "90° counter-clockwise"
    }
  }
  type RotationAngle = RotationAngle.Value

  case class Rotation(angle: RotationAngle, side: SideName) extends Description{
    def asString = "rotated: " + side.toString + " " + RotationAngle.toStr(angle)
  }
  case object NoDescription extends Description { def asString = "" }
  case class InitialDescription(asString: String) extends Description



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