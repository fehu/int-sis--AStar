package feh.tec.rubik.ogl

import feh.util.Path
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{GL11, GL15}
import org.macrogl._
import org.macrogl.ex.IndexBuffer


class ShaderProg(val indices: Array[Int],
                 val vertices: Array[Float],
                 val num_components: Int,
                 val components: Array[(Int, Int)],
                 val vertShaderResource: Path,
                 val fragShaderResource: Path,
                 val shaderConf: ShaderProgramConf,
                 val buffUsage: Int = GL15.GL_STATIC_DRAW)
{
  protected lazy val ibb = BufferUtils.createIntBuffer(indices.length)
  protected lazy val cfb = BufferUtils.createFloatBuffer(vertices.length)

  lazy val vertexBuffer = new AttributeBuffer(
    buffUsage, vertices.length / num_components,
    num_components, components
  )

  lazy val indexBuffer = new ex.IndexBuffer(buffUsage, indices.length)

  lazy val pp = new Program("test")(
    Program.Shader.Vertex  (readResource(vertShaderResource)),
    Program.Shader.Fragment(readResource(fragShaderResource))
  )
  
  
  def init(projectionTransform: Matrix): Unit ={
    ibb.put(indices)
    ibb.flip()

    cfb.put(vertices)
    cfb.flip()

    vertexBuffer.acquire()
    vertexBuffer.send(0, cfb)

    indexBuffer.acquire()
    indexBuffer.send(0, ibb)

    pp.acquire()

    for (_ <- using.program(pp)) {
      def normalize(x: Float, y: Float, z: Float) = {
        val len = x * x + y * y + z * z
        (x / len, y / len, z / len)
      }

      pp.uniform.projection = projectionTransform
      pp.uniform.lightColor = shaderConf.lightColor
      pp.uniform.lightDirection = (normalize _).tupled(shaderConf.lightDirection)
      pp.uniform.ambient = shaderConf.ambient
      pp.uniform.diffuse = shaderConf.diffuse
    }

    GL11.glEnable(GL11.GL_CULL_FACE)
    GL11.glEnable(GL11.GL_DEPTH_TEST)
  }

  def release() = {
    pp.release()
    vertexBuffer.release()
    indexBuffer.release()
  }
  
  def draw(camera: Matrix.Camera, drawGL: IndexBuffer.Access => Unit)
          (implicit gl: Macrogl): Unit =
    for {
      _ <- using.program(pp)
      _ <- using.vertexbuffer(vertexBuffer)
      b <- ex.using.indexbuffer(indexBuffer)
    } {
      gl.checkError()
      gl.clearColor(0.0f, 0.0f, 0.0f, 1.0f)
      raster.clear(Macrogl.COLOR_BUFFER_BIT | Macrogl.DEPTH_BUFFER_BIT)

      pp.uniform.viewTransform = camera.transform
      drawGL(b)
    }


  private def readResource(path: Path) = io.Source.fromURL(getClass.getResource(path.mkString("/"))).mkString
}


case class ShaderProgramConf(lightColor    : (Float, Float, Float),
                             lightDirection: (Float, Float, Float),
                             ambient       : Float,
                             diffuse       : Float )


case class ShaderProgContainer(prog: ShaderProg, drawGL: IndexBuffer.Access => Unit)

/**  */
trait ShadersSupport extends DefaultApp3DExec
{
  
  protected def shaderProgs: Seq[ShaderProgContainer]

  override protected def initApp() = {
    super.initApp()
    shaderProgs.foreach(_.prog.init(projectionTransform))
  }

  override protected def update() = {
    super.update()
    shaderProgs.foreach( c => c.prog.draw(camera, c.drawGL) )
  }

  override protected def terminateApp() = {
    shaderProgs.foreach(_.prog.release())
    super.terminateApp()
  }

}