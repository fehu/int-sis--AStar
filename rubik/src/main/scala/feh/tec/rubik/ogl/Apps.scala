package feh.tec.rubik.ogl

import feh.tec.rubik.ogl.App3DControls._
import org.lwjgl.input.{Mouse, Keyboard}
import org.lwjgl.opengl.{PixelFormat, Display, DisplayMode, ContextAttribs}
import org.macrogl.Matrix

trait DisplayApp extends App {

  def contextAttributes: ContextAttribs
  def displayMode: DisplayMode


  protected def initApp(): Unit ={
    Display.setDisplayMode(new DisplayMode(800, 600))
    Display.create(new PixelFormat, contextAttributes)
  }

  protected def execLoop(): Unit

  protected def terminateApp(): Unit = Display.destroy()

  def run() = {
    initApp()
    execLoop()
    terminateApp()
  }
}

trait App3D extends DisplayApp{

  def projectionTransform: Matrix
  def camera: Matrix.Camera

}

trait DefaultApp3DExec extends App3D{

  def stopCondition: Boolean = Display.isCloseRequested

  protected def update(): Unit

  protected var dtSeconds = 0.0
  protected var prevTime = System.currentTimeMillis

  protected def execLoop() =
    while (!stopCondition) {
      val time = System.currentTimeMillis
      dtSeconds = (time - prevTime) / 1000.0
      prevTime = time

      update()
      Display.update()
    }

}






/*  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 */







trait App3DControls extends DefaultApp3DExec{

  protected var controlHooks: Set[MutableStateHook[_]] = Set()
  protected def processInput(): Unit = {}

  protected def update(): Unit = {
    processInput()
    controlHooks.foreach(_.runHook())
  }

}


object App3DControls{
  
  class MutableState[T](protected var _state: T){
    def set(t: T): Unit = _state = t
    def get = _state
    def state = get
  }

  case class MutableStateHook[T](state: MutableState[T],
                                 hook : T => Unit)
  {
    def runHook() = hook(state.get)

    override def equals(obj: Any): Boolean = canEqual(obj) && (obj match {
      case that: MutableStateHook[_] => this.state == that.state
    })
    
    override def hashCode(): Int = state.hashCode()
  }

  
  case class KeyEvent(key: Int = Keyboard.getEventKey)

  case class MouseClick(button: Int = Mouse.getEventButton,
                        x: Int = Mouse.getEventX,
                        y: Int = Mouse.getEventY )

  case class MousePosition(x: Int = Mouse.getX,
                           y: Int = Mouse.getY)
  {
    def pair = x -> y
  }
}

trait App3DKeyControls extends App3DControls{

  protected def onKeyDown: Map[KeyEvent, () => Unit]
  protected def onKeyPressed: PartialFunction[KeyEvent, Unit]

  override protected def processInput(): Unit = {
    super.processInput()

    if (Keyboard.getEventKeyState) onKeyPressed.lift(KeyEvent())

    onKeyDown.foreach{
      case (KeyEvent(k), f) => if (Keyboard.isKeyDown(k)) f()
    }
  }

}

trait App3DMouseControls extends App3DControls{

  protected def onMouseClick: PartialFunction[MouseClick, Unit]
  protected def mouseControl: PartialFunction[MousePosition, Unit]

  override protected def processInput(): Unit = {
    super.processInput()

    if (Mouse.getEventButtonState) onMouseClick.lift(MouseClick())
    mouseControl.lift(MousePosition())
  }
}

trait App3DFullControls extends App3DKeyControls with App3DMouseControls






/*  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 */







trait App3DExit extends App3DControls{

  def exitKey: Int

  protected lazy val exitRequested = new MutableState(false)
  
  override def stopCondition: Boolean = super.stopCondition || exitRequested.get
}

trait App3DMovement extends App3DControls{

  def cameraSpeed: Float



}
