package feh.tec.rubik.ogl

import feh.tec.rubik.ogl.App3DControls.{MutableStateHook, KeyEvent, MutableState, MousePosition}
import feh.tec.rubik.ogl.test.Cubes._
import org.lwjgl.input.Keyboard
import org.macrogl.Matrix
import Utils.CameraExt

trait FlyingCamera extends App3DFullControls{

  def displayX: Int
  def displayY: Int

  def mouseSensibility: Double




  protected def onMouseClick = Map()
  protected def mouseControl = {
    case MousePosition(x, y) =>
      val xOffset = displayX / 2 - x
      val yOffset = displayY / 2 - y
      camera.setOrientation(xOffset * mouseSensibility, yOffset * mouseSensibility)
  }

  protected val resetRequested  = new MutableState(false)

  protected val movingForward   = new MutableState(false)
  protected val movingBackward  = new MutableState(false)
  protected val movingLeft      = new MutableState(false)
  protected val movingRight     = new MutableState(false)
  protected val movingUp        = new MutableState(false)
  protected val movingDown      = new MutableState(false)

  protected val onKeyDown = Map(
    (KeyEvent(Keyboard.KEY_W),        () => !movingBackward.get)  -> movingForward.set _,
    (KeyEvent(Keyboard.KEY_S),        () => !movingForward.get)   -> movingBackward.set _,
    (KeyEvent(Keyboard.KEY_A),        () => !movingRight.get)     -> movingLeft.set _,
    (KeyEvent(Keyboard.KEY_D),        () => !movingLeft.get)      -> movingRight.set _,
    (KeyEvent(Keyboard.KEY_SPACE),    () => !movingDown.get)      -> movingUp.set _,
    (KeyEvent(Keyboard.KEY_LCONTROL), () => !movingUp.get)        -> movingDown.set _
  )


  def affectCamera(dir: Matrix.Camera => (Double => Unit)) = dir(camera)(cameraSpeed * dtSeconds)
  def ifTrue(f: => Unit): Boolean => Unit = b => if (b) f

  override protected def initApp() = {
    super.initApp()

    controlHooks ++= Seq(
      MutableStateHook(resetRequested, ifTrue( resetCamera() )),
      MutableStateHook(movingForward,  ifTrue( affectCamera(_.moveForward) )),
      MutableStateHook(movingBackward, ifTrue( affectCamera(_.moveBackward) )),
      MutableStateHook(movingRight,    ifTrue( affectCamera(_.moveRight) )),
      MutableStateHook(movingLeft,     ifTrue( affectCamera(_.moveLeft) )),
      MutableStateHook(movingUp,       ifTrue( affectCamera(_.moveUpwards) )),
      MutableStateHook(movingDown,     ifTrue( affectCamera(_.moveDownwards) ))
    )

  }
}
