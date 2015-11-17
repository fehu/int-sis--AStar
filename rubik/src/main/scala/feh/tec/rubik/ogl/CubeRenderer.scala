package feh.tec.rubik.ogl

import feh.tec.rubik.{RubikSubCubesDefault, RubikCube}
import feh.tec.rubik.RubikCube.{Side, SideName}
import feh.tec.rubik.ogl.Render.GLFColor
import org.lwjgl.BufferUtils
import org.lwjgl.input.{Mouse, Keyboard}
import org.lwjgl.opengl._
import org.macrogl._

trait CubeColorScheme[T] extends (T => GLFColor)




/** Renders given Rubik's Cube */
class CubeRenderer[T: CubeColorScheme] extends Renderer[RubikCube[T]]{
  def render(t: RubikCube[T]): Render = ???
}


class SideRenderer[T](implicit val cs: CubeColorScheme[T], sqr: SquareRenderer) extends Renderer[Side[T]]{
  def render(t: Side[T]): Render ={
    val squares =
      for{
        i <- 0 until 3
        j <- 0 until 3
        sCube = t.cubes(i)(j)
      } yield sqr.render(i -> j).color(cs(sCube.label))

    squares.reduceLeft(_ andThen _)
  }

}


class SquareRenderer extends Renderer[(Int, Int)]{

  def render(t: (Int, Int)): Render = Render(GL11.GL_QUADS, {
    val x = t._1
    val y = t._2

    GL11.glVertex3f(0 + x, 0 + y, 0)
    GL11.glVertex3f(0 + x, 1 + y, 0)
    GL11.glVertex3f(1 + x, 1 + y, 0)
    GL11.glVertex3f(1 + x, 0 + y, 0)
  })

}


class RenderExec(render: => Unit, sync: Int = 60){

  def run() = {
    while (!Display.isCloseRequested) {
      try{
        render
        Display.update()
        Display.sync(sync)
      } catch {
        case thr: Throwable => thr.printStackTrace()
      }
    }
    Display.destroy()
  }

}

object DefaultRubikColorScheme extends CubeColorScheme[SideName]{
  import SideName._

  def apply(v1: SideName) = v1 match {
    case Front => (1, 0, 0)
    case Right => (0, 1, 0)
    case Left  => (0, 0, 1)
    case Up    => (1, 1, 0)
    case Down  => (1, 1, 1)
    case Back  => (1, 0.65f, 0)
  }
}

@deprecated("delete it")
object RendererTest extends App{
  val contextAttributes = new ContextAttribs(3, 2)
    .withForwardCompatible(true).withProfileCore(true)

  Display.setDisplayMode(new DisplayMode(800, 600))
  Display.create(new PixelFormat, contextAttributes)



  val ibb = BufferUtils.createIntBuffer(Cube.indices.length)
  ibb.put(Cube.indices)
  ibb.flip()

  val cfb = BufferUtils.createFloatBuffer(Cube.vertices.length)
  cfb.put(Cube.vertices)
  cfb.flip()

  val vertexBuffer = new AttributeBuffer(
    GL15.GL_STATIC_DRAW, Cube.vertices.length / Cube.num_components,
    Cube.num_components, Cube.components)
  vertexBuffer.acquire()
  vertexBuffer.send(0, cfb)

  val indexBuffer = new ex.IndexBuffer(GL15.GL_STATIC_DRAW, Cube.indices.length)
  indexBuffer.acquire()
  indexBuffer.send(0, ibb)

  def readResource(path: String) = io.Source.fromURL(getClass.getResource(path)).mkString

  val pp = new Program("test")(
    Program.Shader.Vertex(
      readResource("/org/macrogl/examples/BasicLighting.vert")),
    Program.Shader.Fragment(
      readResource("/org/macrogl/examples/BasicLighting.frag")))

  pp.acquire()

  val projectionTransform =
    Matrix.perspectiveProjection(50, 800.0 / 600.0, 0.1, 100.0)

  val camera = new Matrix.Camera(0, 0, 4)
  val cameraSpeed = 5.0

  var dtSeconds = 0.0
  var prevTime = System.currentTimeMillis

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

  for (_ <- using.program(pp)) {
    def normalize(x: Float, y: Float, z: Float) = {
      val len = x * x + y * y + z * z
      (x / len, y / len, z / len)
    }

    pp.uniform.projection = projectionTransform
    pp.uniform.lightColor = (1.0f, 1.0f, 1.0f)
    pp.uniform.lightDirection = normalize(0.0f, -1.0f, -1.0f)
    pp.uniform.ambient = 0.05f
    pp.uniform.diffuse = 0.95f
  }

  GL11.glEnable(GL11.GL_CULL_FACE)
  GL11.glEnable(GL11.GL_DEPTH_TEST)

  Mouse.setCursorPosition(400, 300)

  while (!Display.isCloseRequested && !closeRequested) {
    val time = System.currentTimeMillis
    dtSeconds = (time - prevTime) / 1000.0
    prevTime = time

    val stateChanged = processInput()
    if (stateChanged) {
      if (cameraResetRequested) {
        camera.position(0) = 0
        camera.position(1) = 0
        camera.position(2) = 4

        camera.horizontalAngle = 0
        camera.verticalAngle = 0

        cameraResetRequested = false
      }
    }

    // update camera
    if (movingForward) camera.moveForward(cameraSpeed * dtSeconds)
    if (movingBackward) camera.moveBackward(cameraSpeed * dtSeconds)
    if (movingLeft) camera.moveLeft(cameraSpeed * dtSeconds)
    if (movingRight) camera.moveRight(cameraSpeed * dtSeconds)

    val xOffset = 400 - Mouse.getX
    val yOffset = 300 - Mouse.getY
    camera.setOrientation(xOffset * 0.01, yOffset * 0.01) // offsetOrientation

    // update animations
    val angle = time / 1000.0

    import scala.math.{ sin, cos }
    val c = cos(angle)
    val s = sin(angle)

    leftTransform.array(0) = c
    leftTransform.array(2) = s
    leftTransform.array(8) = -s
    leftTransform.array(10) = c

    rightTransform.array(5) = c
    rightTransform.array(6) = s
    rightTransform.array(9) = -s
    rightTransform.array(10) = c

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

    Display.update()
  }

  pp.release()
  vertexBuffer.release()
  indexBuffer.release()
  Display.destroy()




  var movingForward = false
  var movingBackward = false
  var movingLeft = false
  var movingRight = false

  var closeRequested = false
  var cameraResetRequested = false

  def processInput(): Boolean = {
    var stateChanged = false

    while (Keyboard.next()) {
      movingForward = Keyboard.isKeyDown(Keyboard.KEY_W)
      movingBackward = !movingForward && Keyboard.isKeyDown(Keyboard.KEY_S)

      movingLeft = Keyboard.isKeyDown(Keyboard.KEY_A)
      movingRight = !movingLeft && Keyboard.isKeyDown(Keyboard.KEY_D)

      if (Keyboard.getEventKeyState()) {
        stateChanged ||= {
          Keyboard.getEventKey() match {
            case Keyboard.KEY_ESCAPE =>
              closeRequested = true; false

            case Keyboard.KEY_F5 =>
              cameraResetRequested = true; true

            case _ => false
          }
        }
      }
    }
    stateChanged
  }

//  val cube = RubikSubCubesDefault.randomCube()
//  val side = cube.front
//  implicit def colors = DefaultRubikColorScheme
//  implicit val sqRend = new SquareRenderer
//  val r = new SideRenderer[SideName]
//  def render() = {
//    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT)
////    GL11.glLoadIdentity()
//    r.render(side)
//  }
//  while (!Display.isCloseRequested) {
//    render()
//    Display.update()
//  }
//  Display.destroy()
}

/** from https://github.com/storm-enroute/macrogl/blob/master/src/test/scala/org/macrogl/examples/BasicLighting.scala */
object Cube {
  val num_components = 9

  val components = Array((0, 3), (3, 3), (6, 3))

  // position, normal, color
  val vertices = Array[Float](
      // bottom
      -1.0f, -1.0f, -1.0f, 0, -1, 0, 1, 0, 0,
       1.0f, -1.0f, -1.0f, 0, -1, 0, 1, 0, 0,
      -1.0f, -1.0f,  1.0f, 0, -1, 0, 1, 0, 0,
       1.0f, -1.0f,  1.0f, 0, -1, 0, 1, 0, 0,
      // top
      -1.0f, 1.0f, -1.0f, 0, 1, 0, 0, 1, 0,
      -1.0f, 1.0f,  1.0f, 0, 1, 0, 0, 1, 0,
       1.0f, 1.0f, -1.0f, 0, 1, 0, 0, 1, 0,
       1.0f, 1.0f,  1.0f, 0, 1, 0, 0, 1, 0,
      // front
      -1.0f,  1.0f, 1.0f, 0, 0, 1, 0, 0, 1,
      -1.0f, -1.0f, 1.0f, 0, 0, 1, 0, 0, 1,
       1.0f,  1.0f, 1.0f, 0, 0, 1, 0, 0, 1,
       1.0f, -1.0f, 1.0f, 0, 0, 1, 0, 0, 1,
      // back
       1.0f,  1.0f, -1.0f, 0, 0, -1, 1, 1, 0,
       1.0f, -1.0f, -1.0f, 0, 0, -1, 1, 1, 0,
      -1.0f,  1.0f, -1.0f, 0, 0, -1, 1, 1, 0,
      -1.0f, -1.0f, -1.0f, 0, 0, -1, 1, 1, 0,
      // left
      -1.0f,  1.0f,  1.0f, -1, 0, 0, 1, 0, 1,
      -1.0f,  1.0f, -1.0f, -1, 0, 0, 1, 0, 1,
      -1.0f, -1.0f,  1.0f, -1, 0, 0, 1, 0, 1,
      -1.0f, -1.0f, -1.0f, -1, 0, 0, 1, 0, 1,
      // right
      1.0f,  1.0f, -1.0f, 1, 0, 0, 0, 1, 1,
      1.0f,  1.0f,  1.0f, 1, 0, 0, 0, 1, 1,
      1.0f, -1.0f, -1.0f, 1, 0, 0, 0, 1, 1,
      1.0f, -1.0f,  1.0f, 1, 0, 0, 0, 1, 1)

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

/*
object RendererTest extends App{
  Display.setFullscreen(false)
  Display.setDisplayMode(new DisplayMode(800, 600))
  Display.setTitle("RendererTest")
  Display.create()


  GL11.glEnable(GL11.GL_TEXTURE_2D)
  GL11.glShadeModel(GL11.GL_SMOOTH)
  GL11.glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
  GL11.glClearDepth(1.0)
  GL11.glEnable(GL11.GL_DEPTH_TEST)
  GL11.glDepthFunc(GL11.GL_LEQUAL)

  GL11.glMatrixMode(GL11.GL_PROJECTION)
  GL11.glLoadIdentity()


//  GLU.gluPerspective(
//    45.0f,
//    (float)displayMode.getWidth()/(float)      displayMode.getHeight(),
//  0.1f,
//  100.0f)

  GL11.glMatrixMode(GL11.GL_MODELVIEW)
  GL11.glHint(GL11.GL_PERSPECTIVE_CORRECTION_HINT, GL11.GL_NICEST)



  val cube = RubikSubCubesDefault.randomCube()
  val side = cube.front


  implicit def colors = DefaultRubikColorScheme
  implicit val sqRend = new SquareRenderer

  val r = new SideRenderer[SideName]


  def render() = {
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT)
    GL11.glLoadIdentity()
    r.render(side)
  }

  object Exec extends RenderExec(render())

  Exec.run()
}*/
