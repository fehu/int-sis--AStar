package feh.tec.rubik.ogl

import feh.tec.rubik.ogl.RendererTest._
import feh.util.Path
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{GL11, GL15, ContextAttribs, DisplayMode}
import org.macrogl.{using, Program, ex, AttributeBuffer}

/**  */
trait ShaderApp extends App3D
{

  def indices: Array[Int]
  def vertices: Array[Float]

  def num_components: Int
  def components: Array[(Int, Int)]

  def vertShaderResource: Path
  def fragShaderResource: Path

  def shaderConf: ShaderProgramConf

  def buffUsage = GL15.GL_STATIC_DRAW

  protected lazy val ibb = BufferUtils.createIntBuffer(indices.length)
  protected lazy val cfb = BufferUtils.createFloatBuffer(vertices.length)

  protected lazy val vertexBuffer = new AttributeBuffer(
    buffUsage, vertices.length / num_components,
    num_components, components
  )

  protected lazy val indexBuffer = new ex.IndexBuffer(buffUsage, indices.length)

  protected lazy val pp = new Program("test")(
    Program.Shader.Vertex  (readResource(vertShaderResource)),
    Program.Shader.Fragment(readResource(fragShaderResource))
  )

  override protected def initApp() = {
    super.initApp()

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


  override protected def terminateApp() = {
    pp.release()
    vertexBuffer.release()
    indexBuffer.release()

    super.terminateApp()
  }

  private def readResource(path: Path) = io.Source.fromURL(getClass.getResource(path.mkString("/"))).mkString
}

case class ShaderProgramConf(lightColor    : (Float, Float, Float),
                             lightDirection: (Float, Float, Float),
                             ambient       : Float,
                             diffuse       : Float )