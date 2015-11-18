package feh.tec.rubik.ogl

import feh.tec.rubik.{CubeOrientation, Rubik, RubikCube}
import feh.tec.rubik.RubikCube.{WithSideName, WithSideNameWrapper, Corner, SideName}
import feh.tec.rubik.ogl.CubeColorScheme.GLFColor
import feh.util._
import org.macrogl.ex.IndexBuffer
import org.macrogl.{AttributeBuffer, Macrogl, Matrix, Program}


trait CubeColorScheme[T] extends (T => GLFColor){
  def asMap: Map[T, GLFColor]
}

object CubeColorScheme{
  type GLFColor = (Float, Float, Float)
}


/** Renders given Rubik's Cube */
class RubikRender[T: CubeColorScheme: WithSideName](val rubik: Rubik[T], val shader: ShaderProg){
  def defaultColor = (0.5f, 0.5f, 0.5f)

  lazy val shaderContainer = ShaderProgContainer(shader, shadersMap.values.toSeq)

  protected lazy val shadersMap = rubik.cubes.map{
    case ((x, y, z), (c, o)) =>
      val vertices = Cube.coloredVertices(defaultColor, mkMp(c.labels))
      val transform = cubePosition(x, y, z) // todo: use Orientation
      c -> mkShaderInst(vertices, transform)
  }

  private def colors = implicitly[CubeColorScheme[T]]
  private def mkMp(s: Seq[T]) = s.map(t => t.side -> colors(t)).toMap

  protected def mkShaderInst(vertices: Array[Float], transform: Matrix.Plain) = ShaderProgInstanceContainer(
    new shader.Instance(Cube.indices, vertices, Cube.num_components, Cube.components),
    {
      case DrawArg(pp, vertexBuf, b) =>
        pp.uniform.worldTransform = transform
        b.render(Macrogl.TRIANGLES, vertexBuf)
    }
  )

  def cubePosition(x: Int, y: Int, z: Int, c: Double = 2.05) = new Matrix.Plain(
    Array[Double](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        c*x, c*y, -5 + c*z, 1)
  )

}



@deprecated("use `feh.tec.rubik.ogl.RubikRender`")
class CubeRenderer[T: CubeColorScheme](rubik: Rubik[T]){

  def render(arg: DrawArg)  = {
    val cubes = rubik.cubes
    import arg._

    for { ((x, y, z), (c, _/* TODO */)) <- cubes }{

      pp.uniform.worldTransform = cubePosition(x, y, z)
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
  
  lazy val asMap = Map(
    Front  -> (1f, 0f, 0f),
    Right  -> (0f, 1f, 0f),
    Left   -> (0f, 0f, 1f),
    Up     -> (1f, 1f, 0f),
    Down   -> (1f, 1f, 1f),
    Back   -> (1f, 0.65f, 0f)
  )

  def apply(v1: SideName) = asMap(v1)
}


/** from https://github.com/storm-enroute/macrogl/blob/master/src/test/scala/org/macrogl/examples/BasicLighting.scala */
object Cube {
  val num_components = 9

  val components = Array((0, 3), (3, 3), (6, 3))

  // position, normal
  protected val vertices = Array[Float](
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


  def coloredVertices(default: GLFColor, colors: Map[SideName, GLFColor]): Array[Float] =
    coloredVertices(Array(
      colors.getOrElse(SideName.Down,  default),
      colors.getOrElse(SideName.Up,    default),
      colors.getOrElse(SideName.Front, default),
      colors.getOrElse(SideName.Back,  default),
      colors.getOrElse(SideName.Left,  default),
      colors.getOrElse(SideName.Right, default)
    ))

  def coloredVertices(colors: Array[GLFColor]): Array[Float] = {
    val target = Array.ofDim[Float](4*6*num_components)

    for {
      k <- 0 until 6
      i <- k*4 until (k+1)*4
      ii = i*num_components
      num_c = num_components-3
    }{
      for (j <- 0 until num_c) target(ii+j) = vertices(i*num_c+j)
      for (j <- 0 until 3) target(ii+num_c+j) = colors(k).productElement(j).asInstanceOf[Float]
    }

    target
  }

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
