package feh.tec.rubik.ogl.test

import feh.tec.rubik.RubikSubCubesDefault.WithSideNameIdentity
import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl.App3DControls.{MutableStateHook, KeyEvent}
import feh.tec.rubik.ogl._
import feh.tec.rubik.{Rubik, RubikSubCubesDefault}
import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{ContextAttribs, DisplayMode}
import org.macrogl._

object RCube extends ShadersSupport with FlyingCamera with App3DExit{

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

  def resetCamera() = {
    camera.position(0) = 0
    camera.position(1) = 0
    camera.position(2) = 4

    camera.horizontalAngle = 0
    camera.verticalAngle = 0

    resetRequested set false
  }


  implicit def colors = DefaultRubikColorScheme
  val rubik = new Rubik[SideName](RubikSubCubesDefault.cubes)


  val rr = new RubikRender(rubik)

  protected val shaderProgs = rr.shaders

  run()

  override protected def initApp() = {
    super.initApp()

    controlHooks += MutableStateHook(resetRequested, ifTrue( resetCamera() ))
    Mouse.setCursorPosition(displayX / 2, displayY / 2)
  }
}
