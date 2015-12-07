package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.RubikCubeImage.Side
import feh.util._


case class RubikCubeImage[T](sides: Seq[Side[T]]){
  def map[R](f: T => R): RubikCubeImage[R] = copy(sides.map(_.map(f)))

  def merge[T2](that: RubikCubeImage[T2]): RubikCubeImage[(T, T2)] = RubikCubeImage(
    this.sides zip that.sides map { case (Side(c1, _), Side(c2, _)) => Side(c1 zipByKey c2) }
  )
}

object RubikCubeImage{
  case class Side[T](colors: Map[(Int, Int), T], name: Option[SideName] = None){
    def map[R](f: T => R): Side[R] = copy(colors.mapValues(f))
  }

  case class ColorMap[T, C](colorFor: PartialFunction[T, C])
  case class ColorMapCreationError(reason: String) extends RuntimeException("Failed to create color map: " + reason)

  case class ReadSide(name: SideName,
                      flipX: Boolean = false,
                      flipY: Boolean = false,
                      transposed: Boolean = false
                       )
  case class SidesMap(readOrder: Seq[ReadSide])

  object SidesMap{
    def ordering(m: SidesMap): Ordering[SideName] = {
      val sideNames = m.readOrder.map(_.name)
      Ordering.by(sideNames.indexOf[SideName])
    }
  }


  case class CubeSide[C](cubeId: CubeId, orientation: SideName, color: C){
    def id = cubeId -> orientation
  }


  def apply[T](inst: RubikCubeInstance[T]): RubikCubeImage[T] = RubikCubeImage(
    inst.rawSides.toSeq.map{
      case (side, contents) => Side(contents.mapValues(_.selectSideByOrientation(side)), Some(side))
    }
  )

  def toString[T](img: RubikCubeImage[T], sm: SidesMap) =
    img.sides.zip(sm.readOrder).flatMap{
      case (RubikCubeImage.Side(colors, _), readSide) =>
        val cStrs = colors.toSeq.map{ case ((x, y), color) => Seq(x, y, color).mkString(", ") }
        ("-- " + readSide.name) +: cStrs
    }.mkString("\n")

  def toString[T](img: RubikCubeImage[T]) =
    img.sides.flatMap{
      case RubikCubeImage.Side(colors, Some(name)) =>
        val cStrs = colors.toSeq.map{ case ((x, y), color) => Seq(x, y, color).mkString(", ") }
        ("-- " + name) +: cStrs
    }.mkString("\n")

  def groupCubes[T: WithSideName](cubesSides: Iterable[(CubeId, (SideName, T))]): Map[CubeId, CubeWithOrientation[T]] =
    groupCubes(
      cubesSides.toSeq.map{
        case (id,(o, c)) => CubeSide(id, o, c)
      }
    )
  def groupCubes[T: WithSideName](cubesSides: Seq[CubeSide[T]]): Map[CubeId, CubeWithOrientation[T]] =
    cubesSides.groupBy(_.cubeId).mapValues{
      case Seq(CubeSide(_, o1, c1), CubeSide(_, o2, c2), CubeSide(_, o3, c3))=>
        Corner(c1, c2, c3) -> CubeOrientation(o1, o2, o3)
      case Seq(CubeSide(_, o1, c1), CubeSide(_, o2, c2)) =>
        Middle(c1, c2) -> CubeOrientation(o1, o2, null)
      case Seq(CubeSide(_, o, c)) =>
        Center(c) -> CubeOrientation(o, null, null)
    }

  // todo: SidesMap not always used
  def readCubes[C: WithSideName](img: RubikCubeImage[C]): Map[CubeId, CubeWithOrientation[C]] =
  {
    val RubikCubeImage(sides) = img

    val cubesSides = sides flatMap {
      case Side(colors, Some(side)) => mkSide(side, colors)
    }

    groupCubes(cubesSides)
  }

  // todo: SidesMap not always used
  def readCubesWithMap[C: WithSideName](img: RubikCubeImage[C])
                                       (implicit sMap: SidesMap): Map[CubeId, CubeWithOrientation[C]] =
  {
    val RubikCubeImage(sides) = img

    def flip(x: Int) = x match{
      case 2 => 0
      case 1 => 1
      case 0 => 2
    }

    val cubesSides = sMap.readOrder zip sides flatMap {

      case (ReadSide(side, false, false, false), Side(colors, _)) =>
        mkSide(side, colors)

      case (ReadSide(side, flipX, flipY, transpose), Side(colors, _)) =>
        val idsMap = sideCubes(side)
        val flipped =
          if(flipX || flipY) colors.mapKeys{
            case (x, y) => (if (flipX) flip(x) else x, if (flipY) flip(y) else y )
          }
          else colors

        val transposed =
          if(transpose) flipped.mapKeys(_.swap)
          else flipped

        mkSide(side, transposed)
    }

    groupCubes(cubesSides)
  }

  def mkSide[C](side: SideName, colors: Map[(Int, Int), C]) = {
    val idsMap = sideCubes(side)
    colors.map{ case (pos, c) => CubeSide(idsMap(pos), side, c) }
  }


}
