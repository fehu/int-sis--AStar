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

}
