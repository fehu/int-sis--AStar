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

object CubesShader {

  val pathRoot = Path("/org/macrogl/examples/", '/')

  lazy val prog = new ShaderProg(
    Cube.indices,
    Cube.coloredVertices((0.1f, 0.1f, 0.1f), DefaultRubikColorScheme.asMap),
    Cube.num_components,
    Cube.components,
    pathRoot / "BasicLighting.vert",
    pathRoot / "BasicLighting.frag",
    ShaderProgramConf(
      lightColor = (1.0f, 1.0f, 1.0f),
      lightDirection = (0.0f, -1.0f, -1.0f),
      ambient = 0.25f,
      diffuse = 0.95f
    )
  )
}


object Cubes extends ShadersSupport with FlyingCamera with App3DExit{

  val displayX = 800
  val displayY = 600


  val contextAttributes = new ContextAttribs(3, 2).withForwardCompatible(true).withProfileCore(true)
  val displayMode =  new DisplayMode(displayX, displayY)



  val projectionTransform = Matrix.perspectiveProjection(50, 800.0 / 600.0, 0.1, 100.0)
  val camera = new Matrix.Camera(0, 0, 4)

  val cameraSpeed = 5.0


  def exitKey: Int = Keyboard.KEY_ESCAPE

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

  val cRenderer = new CubeRenderer(rubic, CubesShader.prog.pp, CubesShader.prog.vertexBuffer)

  protected val shaderProgs = ShaderProgContainer(CubesShader.prog, cRenderer.render) :: Nil

  run()

  override protected def initApp() = {
    super.initApp()
    Mouse.setCursorPosition(displayX / 2, displayY / 2)
  }
}
