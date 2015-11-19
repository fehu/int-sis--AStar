package feh.tec.rubik.ogl

import feh.tec.rubik.RubikCube
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl.CubeColorScheme.GLFColor
import org.macrogl.{Macrogl, Matrix}


trait CubeColorScheme[T] extends (T => GLFColor){
  def asMap: Map[T, GLFColor]
  def bySide: SideName => GLFColor
}

object CubeColorScheme{
  type GLFColor = (Float, Float, Float)
}


/** Renders given Rubik's Cube */
class RubikRender[T: CubeColorScheme: WithSideName](val rubik: RubikCube[T],
                                                    val shader: ShaderProg,
                                                    projectionTransform: Matrix,
                                                    disable: => Boolean )
{
  def defaultColor = (0.1f, 0.1f, 0.1f)

  lazy val shaderContainer = ShaderProgContainer.create(shader, getShaders)

  protected def getShaders ={
    val h = rHash

    if(currentCubeHash != h && !disable) {
      currentCubeHash = h
      recreateShader()
    }
    else if(!disable && currentShaders.isEmpty) recreateShader()
    else if(disable && currentShaders.nonEmpty) currentShaders = Map()

    currentShaders.values.toSeq
  }

  protected def recreateShader() = {
    currentShaders.foreach(_._2.instance.release())
    currentShaders = shadersMap
    currentShaders.foreach(_._2.instance.init())
  }

  protected var currentShaders = shadersMap
  protected var currentCubeHash = rHash

  protected def shadersMap = rubik.cubes.map{
    case (pos, (c, o)) =>
      val vertices = Cube.coloredVertices(defaultColor, mkMp(c.labels, o))
      val transform = cubePose(pos, o) // todo: use Orientation
      c -> mkShaderInst(vertices, transform)
  }

  private def rHash = rubik.cubes.hashCode()
  private def colors = implicitly[CubeColorScheme[T]]
  private def mkMp(s: Seq[T], o: CubeOrientation) = {
    s .zip(o.toSeq)
      .map{ case (t, orient) => orient -> colors(t) }
      .toMap
  }

  protected def mkShaderInst(vertices: Array[Float], transform: Matrix.Plain) = ShaderProgInstanceContainer(
    new shader.Instance(Cube.indices, vertices, Cube.num_components, Cube.components),
    {
      case DrawArg(pp, vertexBuf, b) =>
        pp.uniform.worldTransform = transform
        b.render(Macrogl.TRIANGLES, vertexBuf)
    }
  )

  def interCubeDist = 2.05

  def cubePose(pos: (Int, Int, Int), o: CubeOrientation) = {

    val (x, y, z) = pos
    def c = interCubeDist

    val res = Matrix.identity
    val offset = 4*3
    res.array(offset)   = c*x
    res.array(offset+1) = c*y
    res.array(offset+2) = -5 + c*z
    res
  }

}



object DefaultRubikColorScheme extends CubeColorScheme[SideName]{
  import SideName._
  
  lazy val asMap = Map(
    Front  -> (1f, 0f, 0f),
    Right  -> (0f, 1f, 0f),
    Left   -> (0f, 0f, 1f),
    Up     -> (1f, 1f, 0f),
    Down   -> (1f, 1f, 1f),
    Back   -> (1f, 0.27f, 0f)
  )

  def bySide = asMap

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
