package feh.tec.rubik.ogl

import org.lwjgl.opengl.GL11

import feh.util._

/** Render an object with OpenGL */
trait Renderer[T] {
  def render(t: T): Render
}

case class Render(mode: Int, // todo
                  renderFunc: () => Unit,
                  beforeRender: Seq[() => Unit],
                  afterRender : Seq[() => Unit] )
{ 
  def apply() = {
    GL11.glBegin(mode)
    asFunc()
    GL11.glEnd()
  } 
  
  def doRender() = apply()

  def asFunc = () => {
    beforeRender.foreach(_())
    renderFunc()
    afterRender.foreach(_())
  }

  def andThen(that: Render): Render = {
    assert(this.mode == that.mode, "cannot merge renderings with different modes")
    Render(mode, { this.asFunc(); that.asFunc() })
  }
}


object Render{

  def apply(mode: Int, render: => Unit): Render = Render(mode, render.lift, Nil, Nil)


  type GLFColor = (Float, Float, Float)


  implicit class RenderOps(r: Render){

    def doBefore(f: => Unit) = r.copy(beforeRender = f.lift +: r.beforeRender )
    def doAfter(f: => Unit)  = r.copy(afterRender  = f.lift +: r.afterRender )

    def translate(x: Float, y: Float, z: Float) = doBefore(GL11.glTranslatef(x, y, z))
    def translate(x: Double, y: Double, z: Double) = doBefore(GL11.glTranslated(x, y, z))

    def rotate(angle: Float, x: Float, y: Float, z: Float) = doBefore(GL11.glRotatef(angle, x, y, z))
    def rotate(angle: Double, x: Double, y: Double, z: Double) = doBefore(GL11.glRotated(angle, x, y, z))

    def color(red: Byte, green: Byte, blue: Byte) = doBefore(GL11.glColor3b(red, green, blue))
    def color(red: Float, green: Float, blue: Float) = doBefore(GL11.glColor3f(red, green, blue))
    def color(c: GLFColor) = doBefore((GL11.glColor3f _).tupled(c))
    def color(red: Double, green: Double, blue: Double) = doBefore(GL11.glColor3d(red, green, blue))
//    def color(color: java.awt.Color) = doBefore(GL11.glColor3d(color.getRed, green, blue))
  }

}
