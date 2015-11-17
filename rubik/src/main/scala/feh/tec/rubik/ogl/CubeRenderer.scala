package feh.tec.rubik.ogl

import feh.tec.rubik.{CubeOrientation, Rubik, RubikCube}
import feh.tec.rubik.RubikCube.SideName
import feh.tec.rubik.ogl.CubeColorScheme.GLFColor
import org.macrogl.ex.IndexBuffer
import org.macrogl.{AttributeBuffer, Macrogl, Matrix, Program}


trait CubeColorScheme[T] extends (T => GLFColor)

object CubeColorScheme{
  type GLFColor = (Float, Float, Float)
}


/** Renders given Rubik's Cube */
class CubeRenderer[T: CubeColorScheme](rubik: Rubik[T], pp: Program, vertexBuffer: AttributeBuffer){

  def render(b: IndexBuffer.Access)  = {
    val cubes = rubik.cubes

    for { ((x, y, z), (c, CubeOrientation(o))) <- cubes }{

      pp.uniform.worldTransform = cubePosition(x, y, z)
      pp.uniform.color = (1f, 1f, 1f)
      b.render(Macrogl.TRIANGLES, vertexBuffer)
    }

  }

  def cubePosition(x: Int, y: Int, z: Int, c: Double = 2.05) = new Matrix.Plain(
    Array[Double](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        c*x, c*y, -5 + c*z, 1)
  )

}



object DefaultRubikColorScheme extends CubeColorScheme[SideName]{
  import SideName._

  def apply(v1: SideName) = v1 match {
    case Front => (1, 0, 0)
    case Right => (0, 1, 0)
    case Left  => (0, 0, 1)
    case Up    => (1, 1, 0)
    case Down  => (1, 1, 1)
    case Back  => (1, 0.65f, 0)
  }
}


/** from https://github.com/storm-enroute/macrogl/blob/master/src/test/scala/org/macrogl/examples/BasicLighting.scala */
object Cube {
  val num_components = 6

  val components = Array((0, 3), (3, 3))

  // position, normal
  val vertices = Array[Float](
      // bottom
      -1.0f, -1.0f, -1.0f, 0, -1, 0,
       1.0f, -1.0f, -1.0f, 0, -1, 0,
      -1.0f, -1.0f,  1.0f, 0, -1, 0,
       1.0f, -1.0f,  1.0f, 0, -1, 0,
      // top
      -1.0f, 1.0f, -1.0f, 0, 1, 0,
      -1.0f, 1.0f,  1.0f, 0, 1, 0,
       1.0f, 1.0f, -1.0f, 0, 1, 0,
       1.0f, 1.0f,  1.0f, 0, 1, 0,
      // front
      -1.0f,  1.0f, 1.0f, 0, 0, 1,
      -1.0f, -1.0f, 1.0f, 0, 0, 1,
       1.0f,  1.0f, 1.0f, 0, 0, 1,
       1.0f, -1.0f, 1.0f, 0, 0, 1,
      // back
       1.0f,  1.0f, -1.0f, 0, 0, -1,
       1.0f, -1.0f, -1.0f, 0, 0, -1,
      -1.0f,  1.0f, -1.0f, 0, 0, -1,
      -1.0f, -1.0f, -1.0f, 0, 0, -1,
      // left
      -1.0f,  1.0f,  1.0f, -1, 0, 0,
      -1.0f,  1.0f, -1.0f, -1, 0, 0,
      -1.0f, -1.0f,  1.0f, -1, 0, 0,
      -1.0f, -1.0f, -1.0f, -1, 0, 0,
      // right
      1.0f,  1.0f, -1.0f, 1, 0, 0,
      1.0f,  1.0f,  1.0f, 1, 0, 0,
      1.0f, -1.0f, -1.0f, 1, 0, 0,
      1.0f, -1.0f,  1.0f, 1, 0, 0)

  // todo ??? no idea what it is
  val indices = Array[Int](
      // bottom
      0, 1, 2, 1, 3, 2,
      // top
      4, 5, 6, 6, 5, 7,
      // front
      8, 9, 10, 9, 11, 10,
      // back
      12, 13, 14, 13, 15, 14,
      // left
      16, 17, 18, 17, 19, 18,
      // right
      20, 21, 22, 21, 23, 22)
}

