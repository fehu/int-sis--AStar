package feh.tec.rubik.ogl.run

import feh.tec.rubik.RubikSubCubesDefault.WithSideNameIdentity
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl.App3DControls.{MutableState, MutableStateHook, KeyEvent}
import feh.tec.rubik.ogl._
import feh.tec.rubik.{MutableRubikCube, RubikSubCubesDefault}
import feh.util.Path
import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{ContextAttribs, DisplayMode}
import org.macrogl._

object RCubeMutableRunner extends App{

  PrepareNatives.andThen{ RCubeMutable.main(args) }

}

object RCubeMutable extends ShadersSupport with FlyingCamera with App3DExit{

  val displayX = 800
  val displayY = 600

  val fps = 30

  val contextAttributes = new ContextAttribs(2, 1)
//    .withForwardCompatible(true)
//    .withProfileCore(true)
//    .withProfileES(true)
  val displayMode =  new DisplayMode(displayX, displayY)



  val projectionTransform = Matrix.perspectiveProjection(50, 800.0 / 600.0, 0.1, 100.0)
  val camera = new Matrix.Camera(0, 0, 4)

  val cameraSpeed = 5.0


  def exitKey = Keyboard.KEY_ESCAPE

  def mouseSensibility = 0.05

  protected lazy val disableRequested = new MutableState(false)

  protected lazy val rotateFontRequested  = new MutableState(false)
  protected lazy val rotateBackRequested  = new MutableState(false)
  protected lazy val rotateRightRequested = new MutableState(false)
  protected lazy val rotateLeftRequested  = new MutableState(false)
  protected lazy val rotateUpRequested    = new MutableState(false)
  protected lazy val rotateDownRequested  = new MutableState(false)

  protected val onKeyPressed: PartialFunction[KeyEvent, Unit] = {
    case KeyEvent(Keyboard.KEY_ESCAPE) => exitRequested.set(true)
    case KeyEvent(Keyboard.KEY_F5)     => resetRequested.set(true)
    case KeyEvent(Keyboard.KEY_F10)    => disableRequested.set(!disableRequested.get)

    case KeyEvent(Keyboard.KEY_NUMPAD5 | Keyboard.KEY_5) => rotateFontRequested  set true
    case KeyEvent(Keyboard.KEY_NUMPAD0 | Keyboard.KEY_0) => rotateBackRequested  set true
    case KeyEvent(Keyboard.KEY_NUMPAD6 | Keyboard.KEY_6) => rotateRightRequested set true
    case KeyEvent(Keyboard.KEY_NUMPAD4 | Keyboard.KEY_4) => rotateLeftRequested  set true
    case KeyEvent(Keyboard.KEY_NUMPAD8 | Keyboard.KEY_8) => rotateUpRequested    set true
    case KeyEvent(Keyboard.KEY_NUMPAD2 | Keyboard.KEY_2) => rotateDownRequested  set true
  }

  def resetCamera() = {
    camera.position(0) = 0
    camera.position(1) = 0
    camera.position(2) = 4

    camera.horizontalAngle = 0
    camera.verticalAngle = 0

    resetRequested set false
  }


  implicit def colors = DefaultRubikColorScheme
  val rubik = new MutableRubikCube[SideName](RubikSubCubesDefault.cubes)



  val shader = Shaders.forGLSL("1.2")
  val rr = new RubikRender(rubik, shader, projectionTransform, disableRequested.get)

  protected val shaderProg = rr.shaderContainer

  run()

  override protected def initApp() = {
    super.initApp()

    def rotationHook(state: MutableState[Boolean], side: SideName) =
      MutableStateHook(state, ifTrue{ rubik.rotate(side); state set false })

    controlHooks ++= Seq(
      MutableStateHook(resetRequested, ifTrue( resetCamera() )),

      rotationHook(rotateFontRequested,  SideName.Front),
      rotationHook(rotateBackRequested,  SideName.Back),
      rotationHook(rotateRightRequested, SideName.Right),
      rotationHook(rotateLeftRequested,  SideName.Left),
      rotationHook(rotateUpRequested,    SideName.Up),
      rotationHook(rotateDownRequested,  SideName.Down)
//      MutableStateHook(rotateFontRequested, ifTrue{ rubik.rotate(SideName.Front); rotateFontRequested set false })
    )
    Mouse.setCursorPosition(displayX / 2, displayY / 2)
  }



  object Shaders{
    private lazy val pathRoot = Path("/org/macrogl/examples/", '/')

    def forGLSL(v: String, extra: Map[String, Any] = Map()) = new ShaderProg(
      pathRoot / v / "BasicLighting.vert",
      pathRoot / v / "BasicLighting.frag",
      ShaderProgramConf(Map(
        "projection"      -> projectionTransform,
        "lightColor"      -> (1.0f, 1.0f, 1.0f),
//        "lightDirection"  -> (0.0f, -1.0f, -1.0f),
        "ambient"         -> 0.1f,
        "diffuse"         -> 0.5f
      ) ++ extra
      )
    )
  }
}
