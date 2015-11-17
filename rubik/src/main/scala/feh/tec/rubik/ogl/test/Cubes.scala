package feh.tec.rubik.ogl.test

import feh.tec.rubik.ogl.App3DControls.{MutableStateHook, MutableState, KeyEvent, MousePosition}
import feh.tec.rubik.ogl._
import feh.util.Path
import Utils.CameraExt

import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{ContextAttribs, DisplayMode}
import org.macrogl._

object Cubes extends ShaderApp with App3DFullControls with App3DExit{

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

  protected val resetRequested  = new MutableState(false)

  protected val movingForward   = new MutableState(false)
  protected val movingBackward  = new MutableState(false)
  protected val movingLeft      = new MutableState(false)
  protected val movingRight     = new MutableState(false)
  protected val movingUp        = new MutableState(false)
  protected val movingDown      = new MutableState(false)
  
  protected val onKeyPressed: PartialFunction[KeyEvent, Unit] = {
    case KeyEvent(Keyboard.KEY_ESCAPE) => exitRequested.set(true)
    case KeyEvent(Keyboard.KEY_F5)     => resetRequested.set(true)
  }

  protected val onKeyDown = Map(
    (KeyEvent(Keyboard.KEY_W),        () => !movingBackward.get)  -> movingForward.set _,
    (KeyEvent(Keyboard.KEY_S),        () => !movingForward.get)   -> movingBackward.set _,
    (KeyEvent(Keyboard.KEY_A),        () => !movingRight.get)     -> movingLeft.set _,
    (KeyEvent(Keyboard.KEY_D),        () => !movingLeft.get)      -> movingRight.set _,
    (KeyEvent(Keyboard.KEY_SPACE),    () => !movingDown.get)      -> movingUp.set _,
    (KeyEvent(Keyboard.KEY_LCONTROL), () => !movingUp.get)        -> movingDown.set _
  )


  def cubePosition(x: Int, y: Int) = new Matrix.Plain(
    Array[Double](
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        2.05*x, 2.05*y, -5, 1)
  )


  def affectCamera(dir: Matrix.Camera => (Double => Unit)) = dir(camera)(cameraSpeed * dtSeconds)
  
  controlHooks ++= Seq(
    MutableStateHook(resetRequested, ifTrue( resetCamera() )),
    MutableStateHook(movingForward,  ifTrue( affectCamera(_.moveForward) )),
    MutableStateHook(movingBackward, ifTrue( affectCamera(_.moveBackward) )),
    MutableStateHook(movingRight,    ifTrue( affectCamera(_.moveRight) )),
    MutableStateHook(movingLeft,     ifTrue( affectCamera(_.moveLeft) )),
    MutableStateHook(movingUp,       ifTrue( affectCamera(_.moveUpwards) )),
    MutableStateHook(movingDown,     ifTrue( affectCamera(_.moveDownwards) ))
  )


  def ifTrue(f: => Unit): Boolean => Unit = b => if (b) f

  def resetCamera() = {
    camera.position(0) = 0
    camera.position(1) = 0
    camera.position(2) = 4

    camera.horizontalAngle = 0
    camera.verticalAngle = 0

    resetRequested set false
  }


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
      pp.uniform.worldTransform = cubePosition(0, 0)
      b.render(Macrogl.TRIANGLES, vertexBuffer)

      pp.uniform.worldTransform = cubePosition(0, 1)
      b.render(Macrogl.TRIANGLES, vertexBuffer)

      pp.uniform.worldTransform = cubePosition(1, 1)
      b.render(Macrogl.TRIANGLES, vertexBuffer)
    }
  }



  run()

  override protected def initApp() = {
    super.initApp()
    Mouse.setCursorPosition(displayX / 2, displayY / 2)
  }
}
