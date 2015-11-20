package feh.tec.rubik.ogl

import feh.tec.rubik.RubikCube
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl.App3DControls.{MutableStateHook, KeyEvent, MutableState}
import feh.util.Path
import org.lwjgl.input.{Mouse, Keyboard}
import org.lwjgl.opengl.DisplayMode

trait RubikCubeTestGL[T] extends ShadersSupport with FlyingCamera with App3DExit{
  val shader: ShaderProg
  val rubik: RubikCube[T]
  implicit def colors: CubeColorScheme[T]
  implicit def withSideName: WithSideName[T]

  protected def initialMouseXShift: Int
  protected def initialMouseYShift: Int

  lazy val displayMode =  new DisplayMode(displayX, displayY)

  protected lazy val disableRequested = new MutableState(false)

  protected lazy val rotateFontRequested  = new MutableState(false)
  protected lazy val rotateBackRequested  = new MutableState(false)
  protected lazy val rotateRightRequested = new MutableState(false)
  protected lazy val rotateLeftRequested  = new MutableState(false)
  protected lazy val rotateUpRequested    = new MutableState(false)
  protected lazy val rotateDownRequested  = new MutableState(false)

  protected lazy val onKeyPressed: PartialFunction[KeyEvent, Unit] = {
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

  lazy val rr = new RubikRender(rubik, shader, projectionTransform, disableRequested.get)

  protected lazy val shaderProg = rr.shaderContainer

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
    )
    Mouse.setCursorPosition(displayX / 2 + initialMouseXShift, displayY / 2 + initialMouseYShift)
  }



  object Shaders{
    private lazy val pathRoot = Path("/feh/tec/rubik/shader", '/')

    def forGLSL(v: String, extra: Map[String, Any] = Map()) = new ShaderProg(
      pathRoot / v / "BasicLighting.vert",
      pathRoot / v / "BasicLighting.frag",
      ShaderProgramConf(Map(
        "projection"      -> projectionTransform,
        "lightColor"      -> (1.0f, 1.0f, 1.0f),
        "ambient"         -> 0.1f,
        "diffuse"         -> 0.5f
      ) ++ extra
      )
    )
  }
}
