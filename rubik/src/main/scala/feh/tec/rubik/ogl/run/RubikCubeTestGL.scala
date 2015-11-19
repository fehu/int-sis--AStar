package feh.tec.rubik.ogl.run

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl._
import feh.tec.rubik.{MutableRubikCube, RubikCubeInstance, RubikSubCubesDefault}
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.ContextAttribs
import org.macrogl._

object RubikCubeTestGLAppRunner extends App{ PrepareNatives.andThen{ RubikCubeTestGLApp.main(args) } }



object RubikCubeTestGLApp extends RubikCubeTestGL[SideName]{
  val displayX = 800
  val displayY = 600

  val fps = 30

  val contextAttributes = new ContextAttribs(2, 1)
  //    .withForwardCompatible(true)
  //    .withProfileCore(true)
  //    .withProfileES(true)

  val projectionTransform = Matrix.perspectiveProjection(50, displayX.toFloat / displayY, 0.1, 100.0)
  val camera = new Matrix.Camera(8, 8, 8)

  def cameraSpeed = 5.0
  def mouseSensibility = 0.05
  def exitKey = Keyboard.KEY_ESCAPE


  val shader: ShaderProg = Shaders.forGLSL("1.2")
  //        "lightDirection"  -> (0.0f, -1.0f, -1.0f),

  implicit def withSideName = RubikSubCubesDefault.WithSideNameIdentity
  implicit def colors = DefaultRubikColorScheme

  val mutRubik = new MutableRubikCube[SideName](RubikSubCubesDefault.cubes)
  val rubik = new RubikCubeInstance.MutableContainer(mutRubik.snapshot)

  protected def initialMouseXShift = -displayX / 100
  protected def initialMouseYShift = -displayY / 100

  run()

}
