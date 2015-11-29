package feh.tec.rubik

import feh.tec.rubik.RubikCube.{CubeId, SideName}
import feh.tec.rubik.RubikCube.SideName._
import feh.tec.rubik.TestUtils.CubeDescriptor

object RotationManualTest {

  /** rotated: Front 90. */
  def rotation_1: CubeDescriptor = Map(
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
      (0, 2) -> Up,
      (1, 2) -> Right,
      (2, 2) -> Right,
      (0, 1) -> Up,
      (1, 1) -> Right,
      (2, 1) -> Right,
      (0, 0) -> Up,
      (1, 0) -> Right,
      (2, 0) -> Right),
    Left -> Map(
      (0, 2) -> Left,
      (1, 2) -> Left,
      (2, 2) -> Down,
      (0, 1) -> Left,
      (1, 1) -> Left,
      (2, 1) -> Down,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Down),
    Up -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Up,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Up,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Left),
    Down -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Right,
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

  /** rotated: Right 90 */
  def rotation_2: CubeDescriptor = Map(
    Front -> Map(
      (0, 2) -> Front,
      (1, 2) -> Front,
      (2, 2) -> Down,
      (0, 1) -> Front,
      (1, 1) -> Front,
      (2, 1) -> Down,
      (0, 0) -> Front,
      (1, 0) -> Front,
      (2, 0) -> Down),
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
      (2, 2) -> Front,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Front,
      (0, 0) -> Up,
      (1, 0) -> Up,
      (2, 0) -> Front),
    Down -> Map(
      (0, 2) -> Down,
      (1, 2) -> Down,
      (2, 2) -> Back,
      (0, 1) -> Down,
      (1, 1) -> Down,
      (2, 1) -> Back,
      (0, 0) -> Down,
      (1, 0) -> Down,
      (2, 0) -> Back),
    Back -> Map(
      (0, 2) -> Up,
      (1, 2) -> Back,
      (2, 2) -> Back,
      (0, 1) -> Up,
      (1, 1) -> Back,
      (2, 1) -> Back,
      (0, 0) -> Up,
      (1, 0) -> Back,
      (2, 0) -> Back)
  )


  /** rotated: Front 180. */
  def rotation_3: CubeDescriptor = Map(
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
      (0, 2) -> Left,
      (1, 2) -> Right,
      (2, 2) -> Right,
      (0, 1) -> Left,
      (1, 1) -> Right,
      (2, 1) -> Right,
      (0, 0) -> Left,
      (1, 0) -> Right,
      (2, 0) -> Right),
    Left -> Map(
      (0, 2) -> Left,
      (1, 2) -> Left,
      (2, 2) -> Right,
      (0, 1) -> Left,
      (1, 1) -> Left,
      (2, 1) -> Right,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Right),
    Up -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Up,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Up,
      (0, 0) -> Down,
      (1, 0) -> Down,
      (2, 0) -> Down),
    Down -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Up,
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


  /** rotated: Front 90, Back 90. */
  def rotation_4: CubeDescriptor = Map(
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
      (0, 2) -> Up,
      (1, 2) -> Right,
      (2, 2) -> Down,
      (0, 1) -> Up,
      (1, 1) -> Right,
      (2, 1) -> Down,
      (0, 0) -> Up,
      (1, 0) -> Right,
      (2, 0) -> Down),
    Left -> Map(
      (0, 2) -> Up,
      (1, 2) -> Left,
      (2, 2) -> Down,
      (0, 1) -> Up,
      (1, 1) -> Left,
      (2, 1) -> Down,
      (0, 0) -> Up,
      (1, 0) -> Left,
      (2, 0) -> Down),
    Up -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Right,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Up,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Left),
    Down -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Right,
      (0, 1) -> Down,
      (1, 1) -> Down,
      (2, 1) -> Down,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Left),
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


  /** rotated: Front 90, Right 90. */
  def rotation_5: CubeDescriptor = Map(
    Front -> Map(
      (0, 2) -> Front,
      (1, 2) -> Front,
      (2, 2) -> Right,
      (0, 1) -> Front,
      (1, 1) -> Front,
      (2, 1) -> Down,
      (0, 0) -> Front,
      (1, 0) -> Front,
      (2, 0) -> Down),
    Right -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Up,
      (0, 1) -> Right,
      (1, 1) -> Right,
      (2, 1) -> Right,
      (0, 0) -> Right,
      (1, 0) -> Right,
      (2, 0) -> Right),
    Left -> Map(
      (0, 2) -> Left,
      (1, 2) -> Left,
      (2, 2) -> Down,
      (0, 1) -> Left,
      (1, 1) -> Left,
      (2, 1) -> Down,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Down),
    Up -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Front,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Front,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Front),
    Down -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Back,
      (0, 1) -> Down,
      (1, 1) -> Down,
      (2, 1) -> Back,
      (0, 0) -> Down,
      (1, 0) -> Down,
      (2, 0) -> Back),
    Back -> Map(
      (0, 2) -> Left,
      (1, 2) -> Back,
      (2, 2) -> Back,
      (0, 1) -> Up,
      (1, 1) -> Back,
      (2, 1) -> Back,
      (0, 0) -> Up,
      (1, 0) -> Back,
      (2, 0) -> Back)
  )


  /** rotated: Front 90, Back 90, Right 90. */
  def rotation_6: CubeDescriptor = Map(
    Front -> Map(
      (0, 2) -> Front,
      (1, 2) -> Front,
      (2, 2) -> Right,
      (0, 1) -> Front,
      (1, 1) -> Front,
      (2, 1) -> Down,
      (0, 0) -> Front,
      (1, 0) -> Front,
      (2, 0) -> Left),
    Right -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Up,
      (0, 1) -> Right,
      (1, 1) -> Right,
      (2, 1) -> Right,
      (0, 0) -> Down,
      (1, 0) -> Down,
      (2, 0) -> Down),
    Left -> Map(
      (0, 2) -> Up,
      (1, 2) -> Left,
      (2, 2) -> Down,
      (0, 1) -> Up,
      (1, 1) -> Left,
      (2, 1) -> Down,
      (0, 0) -> Up,
      (1, 0) -> Left,
      (2, 0) -> Down),
    Up -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Front,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Front,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Front),
    Down -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Back,
      (0, 1) -> Down,
      (1, 1) -> Down,
      (2, 1) -> Back,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Back),
    Back -> Map(
      (0, 2) -> Left,
      (1, 2) -> Back,
      (2, 2) -> Back,
      (0, 1) -> Up,
      (1, 1) -> Back,
      (2, 1) -> Back,
      (0, 0) -> Right,
      (1, 0) -> Back,
      (2, 0) -> Back)
  )



  /** rotated: Front 90, Right 90, Back 90. */
  def rotation_7: CubeDescriptor = Map(
    Front -> Map(
      (0, 2) -> Front,
      (1, 2) -> Front,
      (2, 2) -> Right,
      (0, 1) -> Front,
      (1, 1) -> Front,
      (2, 1) -> Down,
      (0, 0) -> Front,
      (1, 0) -> Front,
      (2, 0) -> Down),
    Right -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Back,
      (0, 1) -> Right,
      (1, 1) -> Right,
      (2, 1) -> Down,
      (0, 0) -> Right,
      (1, 0) -> Right,
      (2, 0) -> Down),
    Left -> Map(
      (0, 2) -> Front,
      (1, 2) -> Left,
      (2, 2) -> Down,
      (0, 1) -> Up,
      (1, 1) -> Left,
      (2, 1) -> Down,
      (0, 0) -> Up,
      (1, 0) -> Left,
      (2, 0) -> Down),
    Up -> Map(
      (0, 2) -> Up,
      (1, 2) -> Right,
      (2, 2) -> Right,
      (0, 1) -> Up,
      (1, 1) -> Up,
      (2, 1) -> Front,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Front),
    Down -> Map(
      (0, 2) -> Right,
      (1, 2) -> Right,
      (2, 2) -> Back,
      (0, 1) -> Down,
      (1, 1) -> Down,
      (2, 1) -> Back,
      (0, 0) -> Left,
      (1, 0) -> Left,
      (2, 0) -> Left),
    Back -> Map(
      (0, 2) -> Up,
      (1, 2) -> Up,
      (2, 2) -> Left,
      (0, 1) -> Back,
      (1, 1) -> Back,
      (2, 1) -> Back,
      (0, 0) -> Back,
      (1, 0) -> Back,
      (2, 0) -> Back)
  )


}
