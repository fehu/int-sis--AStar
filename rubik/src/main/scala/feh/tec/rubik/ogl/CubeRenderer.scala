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

