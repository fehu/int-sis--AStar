package feh.tec.rubik.ogl.test

import feh.tec.rubik.RubikSubCubesDefault.WithSideNameIdentity
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl.App3DControls.KeyEvent
import feh.tec.rubik.ogl.{Cube, _}
import feh.tec.rubik.{Rubik, RubikSubCubesDefault}
import feh.util.Path
import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{ContextAttribs, DisplayMode}
import org.macrogl._

object Cubes extends ShaderApp with FlyingCamera with App3DExit{

  val displayX = 800
  val displayY = 600


  val contextAttributes = new ContextAttribs(3, 2).withForwardCompatible(true).withProfileCore(true)
  val displayMode =  new DisplayMode(displayX, displayY)



  val projectionTransform = Matrix.perspectiveProjection(50, 800.0 / 600.0, 0.1, 100.0)
  val camera = new Matrix.Camera(0, 0, 4)

  val cameraSpeed = 5.0


  def exitKey: Int = Keyboard.KEY_ESCAPE

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
    ambient = 0.25f,
    diffuse = 0.95f
  )

  def mouseSensibility = 0.05

  protected val onKeyPressed: PartialFunction[KeyEvent, Unit] = {
    case KeyEvent(Keyboard.KEY_ESCAPE) => exitRequested.set(true)
    case KeyEvent(Keyboard.KEY_F5)     => resetRequested.set(true)
  }


  def cubePosition(x: Int, y: Int) = new Matrix.Plain(
    Array[Double](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        2.05*x, 2.05*y, -5, 1)
  )

  def resetCamera() = {
    camera.position(0) = 0
    camera.position(1) = 0
    camera.position(2) = 4

    camera.horizontalAngle = 0
    camera.verticalAngle = 0

    resetRequested set false
  }

  implicit def colors = DefaultRubikColorScheme

  val rubic = new Rubik[SideName](RubikSubCubesDefault.cubes)

  val cRenderer = new CubeRenderer(rubic, pp, vertexBuffer)


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

      cRenderer.render(b)

    }
  }



  run()

  override protected def initApp() = {
    super.initApp()
    Mouse.setCursorPosition(displayX / 2, displayY / 2)
  }
}
