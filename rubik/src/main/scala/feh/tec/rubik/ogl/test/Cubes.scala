package feh.tec.rubik.ogl.test

import feh.tec.rubik.ogl.App3DControls.MousePosition
import feh.tec.rubik.ogl.{App3DFullControls, Cube, ShaderApp, ShaderProgramConf}
import feh.util.Path
import org.lwjgl.opengl.{ContextAttribs, DisplayMode}
import org.macrogl._

object Cubes extends ShaderApp with App3DFullControls{

  val displayX = 800
  val displayY = 600


  val contextAttributes = new ContextAttribs(3, 2).withForwardCompatible(true).withProfileCore(true)
  val displayMode =  new DisplayMode(displayX, displayY)



  val projectionTransform = Matrix.perspectiveProjection(50, 800.0 / 600.0, 0.1, 100.0)
  val camera = new Matrix.Camera(0, 0, 4)



  def indices        = Cube.indices
  def vertices       = Cube.vertices
  def num_components = Cube.num_components
  def components     = Cube.components

  val pathRoot = Path("/org/macrogl/examples/", '/')

  val vertShaderResource = pathRoot / "BasicLighting.vert"
  val fragShaderResource = pathRoot / "BasicLighting.frag"

  println("vertShaderResource = " + vertShaderResource.mkString("/"))
  println("fragShaderResource = " + fragShaderResource.mkString("/"))

  val shaderConf = ShaderProgramConf(
    lightColor = (1.0f, 1.0f, 1.0f),
    lightDirection = (0.0f, -1.0f, -1.0f),
    ambient = 0.05f,
    diffuse = 0.95f
  )

  protected def onMouseClick = Map()
  protected def mouseControl = {
    case MousePosition(x, y) =>
      val xOffset = displayX / 2 - x
      val yOffset = displayY / 2 - y
      camera.setOrientation(xOffset * 0.01, yOffset * 0.01)
  }

  protected def onKeyDown = Map()
  protected def onKeyPressed = Map()


  val leftTransform = new Matrix.Plain(
    Array[Double](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        -3, 0, -5, 1))

  val rightTransform = new Matrix.Plain(
    Array[Double](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        3, 0, -5, 1))


  override protected def update() = {
    super.update()

    // draw
    val gl = implicitly[Macrogl]
    for {
      _ <- using.program(pp)
      _ <- using.vertexbuffer(vertexBuffer)
      b <- ex.using.indexbuffer(indexBuffer)
    } {
      gl.checkError()
      gl.clearColor(0.0f, 0.0f, 0.0f, 1.0f)
      raster.clear(Macrogl.COLOR_BUFFER_BIT | Macrogl.DEPTH_BUFFER_BIT)

      pp.uniform.viewTransform = camera.transform
      pp.uniform.worldTransform = leftTransform
      b.render(Macrogl.TRIANGLES, vertexBuffer)

      pp.uniform.worldTransform = rightTransform
      b.render(Macrogl.TRIANGLES, vertexBuffer)
    }
  }




  run()
}
