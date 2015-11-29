package feh.tec.rubik

import feh.tec.rubik.RubikCube.{WithSideName, CubeId, SideName}
import feh.tec.rubik.RubikCube.SideName._

import scala.language.higherKinds

object TestUtils {

  type SomeCubeDescriptor[+T] = Map[SideName, Map[(Int, Int), T]]
  type CubeDescriptor = SomeCubeDescriptor[SideName]

  trait MkCube[T, C <: RubikCube[T, C]]{ def mkCube(mp: SomeCubeDescriptor[T])(implicit wsn: WithSideName[T]): C }

  object MkCube{

    implicit def RubikInstance[T]: MkCube[T, RubikCubeInstance[T]] = new MkCube[T, RubikCubeInstance[T]]{
      def mkCube(mp: SomeCubeDescriptor[T])(implicit wsn: WithSideName[T]): RubikCubeInstance[T] =
        CreateRubikInstance(mp, None, RubikCubeInstance.NoDescription)
    }

  }


  def mkCube[T: WithSideName, C <: RubikCube[T, C]](mp: SomeCubeDescriptor[T])
                                                   (implicit iMkCube: MkCube[T, C]) =
    implicitly[MkCube[T, C]].mkCube(mp)

  lazy val solvedCube: CubeDescriptor = Map(
    Front -> Map(
      (0, 2) -> Front,
      (1, 2) -> Front,
      (2, 2) -> Front,
      (0, 1) -> Front,
      (1, 1) -> Front,
      (2, 1) -> Front,
      (0, 0) -> Front,
      (1, 0) -> Front,
      (2, 0) -> Front),
    Right -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Right,
      (0, 1) -> Right,
      (1, 1) -> Right,
      (2, 1) -> Right,
      (0, 0) -> Right,
      (1, 0) -> Right,
      (2, 0) -> Right),
    Left -> Map(
      (0, 2) -> Left,
      (1, 2) -> Left,
      (2, 2) -> Left,
      (0, 1) -> Left,
      (1, 1) -> Left,
      (2, 1) -> Left,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Left),
    Up -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Up,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Up,
      (0, 0) -> Up,
      (1, 0) -> Up,
      (2, 0) -> Up),
    Down -> Map(
      (0, 2) -> Down,
      (1, 2) -> Down,
      (2, 2) -> Down,
      (0, 1) -> Down,
      (1, 1) -> Down,
      (2, 1) -> Down,
      (0, 0) -> Down,
      (1, 0) -> Down,
      (2, 0) -> Down),
    Back -> Map(
      (0, 2) -> Back,
      (1, 2) -> Back,
      (2, 2) -> Back,
      (0, 1) -> Back,
      (1, 1) -> Back,
      (2, 1) -> Back,
      (0, 0) -> Back,
      (1, 0) -> Back,
      (2, 0) -> Back)
  )

}
