package feh.tec.rubik.ogl

import org.macrogl.Matrix

object Utils {

  implicit class CameraExt(c: Matrix.Camera){

    def upwardsDirection = {
      val inv = c.invertedOrientation
      Array(
        inv(0, 3) + inv(0, 1),
        inv(1, 3) + inv(1, 1),
        inv(2, 3) + inv(2, 1))
    }

    def moveUpwards(distance: Double): Unit   = moveDir(upwardsDirection,  distance)
    def moveDownwards(distance: Double): Unit = moveDir(upwardsDirection, -distance)


    private def moveDir(dir: Array[Double], distance: Double) = for (i <- 0 until 3) c.position(i) += dir(i) * distance

  }

  def rotateXMatrix(s: Double, c: Double) = new Matrix.Plain(
    Array[Double](
      1, 0, 0, 0,
      0, c, -s, 0,
      0, s, c, 0,
      0, 0, 0, 1)
  )

  def rotateYMatrix(s: Double, c: Double) = new Matrix.Plain(
    Array[Double](
        c, 0, s, 0,
        0, 1, 0, 0,
        -s, 0, c, 0,
        0, 0, 0, 1)
  )

  def rotateZMatrix(s: Double, c: Double) = new Matrix.Plain(
    Array[Double](
        c, -s, 0, 0,
        s, c, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1)
  )




  sealed trait HalfPiMultAngle{
    def plus: HalfPiMultAngle
    def minus: HalfPiMultAngle
  }

  object HalfPiMultAngle{
    implicit class HalfPiMultAngleOps(a: HalfPiMultAngle){
      def sin = a match {
        case Angle0 | Angle180 =>  0d
        case Angle90           =>  1d
        case Angle270          => -1d
      }
      def cos = a match {
        case Angle90 | Angle270 =>  0d
        case Angle0             =>  1d
        case Angle180           => -1d
      }
    }
  }


  case object Angle0 extends HalfPiMultAngle{
    def plus = Angle90
    def minus = Angle270
  }
  case object Angle90 extends HalfPiMultAngle{
    def plus = Angle180
    def minus = Angle0
  }
  case object Angle180 extends HalfPiMultAngle{
    def plus = Angle270
    def minus = Angle90
  }
  case object Angle270 extends HalfPiMultAngle{
    def plus = Angle0
    def minus = Angle180
  }

}
