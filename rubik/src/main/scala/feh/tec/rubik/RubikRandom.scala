package feh.tec.rubik

import feh.tec.rubik.RubikCube.SideName

import scala.util.Random


object RubikRandom {
  private lazy val sides = SideName.values.toSeq

  def randomSide(): SideName = sides(Random.nextInt(6))
  def randomSides(): Stream[SideName] = randomSide #:: randomSides

  def randomSides(n: Int): Seq[SideName] = randomSides() take n


  def randomRotationCube[T, C <: RubikCube[T, C]](c: C, n: Int): C = c.rotate(randomSides(n): _*)

  implicit class CubeRandomWrapper[T, C <: RubikCube[T, C]](c: C){
    def randomRotations(n: Int = CubeRandomWrapper.maxRandomRotations): C = randomRotationCube[T, C](c, n)
  }
  object CubeRandomWrapper {
    val maxRandomRotations = 100
  }

}
