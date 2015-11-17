package feh.tec.rubik.ogl

import feh.tec.rubik.{RubikSubCubesDefault, RubikCube}
import feh.tec.rubik.RubikCube.{Side, SideName}
import feh.tec.rubik.ogl.Render.GLFColor
import org.lwjgl.opengl.{DisplayMode, Display, GL11}


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
}