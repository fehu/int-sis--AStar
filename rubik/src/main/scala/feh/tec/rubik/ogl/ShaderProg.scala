package feh.tec.rubik.ogl

import java.util.UUID

import feh.util._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{GL11, GL15}
import org.macrogl._
import org.macrogl.ex.IndexBuffer


class ShaderProg(
                 val vertShaderResource: Path,
                 val fragShaderResource: Path,
                 val shaderConf: ShaderProgramConf,
                 val buffUsage: Int = GL15.GL_STATIC_DRAW,
                 val name: String = UUID.randomUUID().toString)
{


  protected[ogl] lazy val pp = new Program(name)(
    Program.Shader.Vertex  (readResource(vertShaderResource)),
    Program.Shader.Fragment(readResource(fragShaderResource))
  )
  
  def init(): Unit ={
    pp.acquire()

    for (_ <- using.program(pp)) {
      def normalize(x: Float, y: Float, z: Float) = {
        val len = x * x + y * y + z * z
        (x / len, y / len, z / len)
      }

      shaderConf.byName.foreach{ case (nme, x) => pp.uniform.updateDynamic(nme)(x) }
    }

    GL11.glEnable(GL11.GL_CULL_FACE)
    GL11.glEnable(GL11.GL_DEPTH_TEST)
  }

  def release(): Unit ={
    pp.release()
  }

  class Instance(val indices: Array[Int],
                 val vertices: Array[Float],
                 val num_components: Int,
                 val components: Array[(Int, Int)])
  {

    protected[ogl] lazy val ibb = BufferUtils.createIntBuffer(indices.length)
    protected[ogl] lazy val cfb = BufferUtils.createFloatBuffer(vertices.length)

    protected[ogl] lazy val vertexBuffer = new AttributeBuffer(
      buffUsage, vertices.length / num_components,
      num_components, components
    )

    protected[ogl] lazy val indexBuffer = new ex.IndexBuffer(buffUsage, indices.length)

    def init(): Unit ={
      ibb.put(indices)
      ibb.flip()

      cfb.put(vertices)
      cfb.flip()

      vertexBuffer.acquire()
      vertexBuffer.send(0, cfb)

      indexBuffer.acquire()
      indexBuffer.send(0, ibb)

    }

    def release() = {
      vertexBuffer.release()
      indexBuffer.release()
    }
  }


  private def readResource(path: Path) = io.Source.fromURL(getClass.getResource(path.mkString("/"))).mkString
}


case class ShaderProgramConf(byName: (String, Any)*)


case class DrawArg(pp: Program, vertexBuffer: AttributeBuffer, b: IndexBuffer.Access)

case class ShaderProgInstanceContainer(instance: ShaderProg#Instance, doDraw: DrawArg => Unit)
case class ShaderProgContainer(prog: ShaderProg, instances: () => Seq[ShaderProgInstanceContainer])
object ShaderProgContainer{
  def create(prog: ShaderProg, instances: => Seq[ShaderProgInstanceContainer]): ShaderProgContainer =
    ShaderProgContainer(prog, () => instances)
}

/**  */
trait ShadersSupport extends DefaultApp3DExec
{
  
  protected def shaderProg: ShaderProgContainer

  override protected def initApp() = {
    super.initApp()
    shaderProg.prog.init()
    shaderProg.instances().foreach(_.instance.init())
  }

  override protected def update() = {
    super.update()
    draw(shaderProg)
  }

  override protected def terminateApp() = {
    shaderProg.instances().foreach(_.instance.release())
    shaderProg.prog.release()
    super.terminateApp()
  }

  def draw(c: ShaderProgContainer)(implicit gl: Macrogl): Unit ={
    raster.clear(Macrogl.COLOR_BUFFER_BIT | Macrogl.DEPTH_BUFFER_BIT)
    for {
      _ <- using.program(c.prog.pp)
      ShaderProgInstanceContainer(i, doDraw) <- c.instances()
    }
    {
      for {
        _ <- using.vertexbuffer(i.vertexBuffer)
        b <- ex.using.indexbuffer(i.indexBuffer)
      } {
        gl.checkError()
        gl.clearColor(0.0f, 0.0f, 0.0f, 1.0f)

        c.prog.pp.uniform.viewTransform = camera.transform
        doDraw(DrawArg(c.prog.pp, i.vertexBuffer, b))
      }
    }
  }

}