package feh.tec.rubik

import feh.tec.rubik.RubikCube.SideName
import feh.tec.rubik.TestUtils._
import feh.tec.rubik.RubikSubCubesDefault.WithSideNameIdentity
import org.specs2.Specification

abstract class RubikCubeManualSpec[C[_] <: RubikCube[_]](name: String) extends Specification { def is = s2"""
  ${("Rubik's Cube representation: " + name).name}

  -- Tests Predefined Examples --


* Let's take a solved cube and test some rotations

  Front 90                                          $test_rot_01
  Right 90                                          $test_rot_02
  Front 180                                         $test_rot_03
  Front 90, Back 90                                 $test_rot_04
  Front 90, Right 90                                $test_rot_05
  Front 90, Back 90, Right 90                       $test_rot_06
  Front 90, Right 90, Back 90                       $test_rot_07


"""

  lazy val initialCubeInstance = mkCube(TestUtils.solvedCube)

  def test_rot_01 = tstCube(RotationManualTest.rotation_1, SideName.Front)
  def test_rot_02 = tstCube(RotationManualTest.rotation_2, SideName.Right)
  def test_rot_03 = tstCube(RotationManualTest.rotation_3, SideName.Front, SideName.Front)
  def test_rot_04 = tstCube(RotationManualTest.rotation_4, SideName.Front, SideName.Back)
  def test_rot_05 = tstCube(RotationManualTest.rotation_5, SideName.Front, SideName.Right)
  def test_rot_06 = tstCube(RotationManualTest.rotation_6, SideName.Front, SideName.Back, SideName.Right)
  def test_rot_07 = tstCube(RotationManualTest.rotation_7, SideName.Front, SideName.Right, SideName.Back)


  private def tstCube(exp: CubeDescriptor, rots: SideName*) = (initialCubeInstance /: rots)(_ rotate _) === mkCube(exp)

}


class RubikCubeInstanceManualSpec
  extends RubikCubeManualSpec[RubikCubeInstance]("RubikCubeInstance")