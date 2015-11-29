package feh.tec.rubik

import feh.tec.rubik.RubikCube.{CubeWithOrientation, SideName}
import feh.tec.rubik.TestUtils._
import feh.tec.rubik.RubikSubCubesDefault.WithSideNameIdentity
import feh.tec.rubik.solve.RubikCubeHeuristics.DistanceMeasure
import org.specs2.Specification

import scala.collection.mutable

abstract class RubikCubeManualSpec[C[_] <: RubikCube[_]: MkCube](name: String) extends Specification { def is = s2"""
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


* Heuristic: DistanceMeasure

  Solved => 0                                       $test_H_DM_01
  Front 90 => 20                                    $test_G_DM_02
  Right 90 => 20                                    $test_G_DM_03
  Front 180 => 24                                   $test_G_DM_04
  Front 90, Back 90 => 40                           $test_G_DM_05
  Front 90, Right 90 => 35                          $test_G_DM_06
  Front 90, Back 90, Right 90 => 50                 $test_G_DM_07
  Front 90, Right 90, Back 90 => 46                 $test_G_DM_08

"""

/*

   */

  lazy val initialCubeInstance = mkCubeH(TestUtils.solvedCube)

  def test_rot_01 = tstCube(RotationManualTest.rotation_1, SideName.Front)
  def test_rot_02 = tstCube(RotationManualTest.rotation_2, SideName.Right)
  def test_rot_03 = tstCube(RotationManualTest.rotation_3, SideName.Front, SideName.Front)
  def test_rot_04 = tstCube(RotationManualTest.rotation_4, SideName.Front, SideName.Back)
  def test_rot_05 = tstCube(RotationManualTest.rotation_5, SideName.Front, SideName.Right)
  def test_rot_06 = tstCube(RotationManualTest.rotation_6, SideName.Front, SideName.Back, SideName.Right)
  def test_rot_07 = tstCube(RotationManualTest.rotation_7, SideName.Front, SideName.Right, SideName.Back)


  def test_H_DM_01 = tstDistanceMeasure(TestUtils.solvedCube, 0)
  def test_G_DM_02 = tstDistanceMeasure(RotationManualTest.rotation_1, 12 + 8)
  def test_G_DM_03 = tstDistanceMeasure(RotationManualTest.rotation_2, 12 + 8)
  def test_G_DM_04 = tstDistanceMeasure(RotationManualTest.rotation_3, 12 + 4*2 + 4)
  def test_G_DM_05 = tstDistanceMeasure(RotationManualTest.rotation_4, (12 + 8) * 2)
  def test_G_DM_06 = tstDistanceMeasure(RotationManualTest.rotation_5, 22 + 7 + 3 + 2 + 1)
  def test_G_DM_07 = tstDistanceMeasure(RotationManualTest.rotation_6, 32 + 8 + 3 + 1 + 1 + 2 + 2 + 1)
  def test_G_DM_08 = tstDistanceMeasure(RotationManualTest.rotation_7, 30 + 7 + 1 + 2 + 1 + 2 + 2 + 1)


  private lazy val cubesHash = mutable.HashMap.empty[CubeDescriptor, C[SideName]]
  private def mkCubeH(cd: CubeDescriptor): C[SideName] = cubesHash.getOrElseUpdate(cd, mkCube(cd))

  private def tstCube(exp: CubeDescriptor, rots: SideName*) = {
    val r = (initialCubeInstance /: rots){ case (c, s) => c.rotate(s).asInstanceOf[C[SideName]] }

    r === mkCubeH(exp)
  }

  private def tstDistanceMeasure(cd: CubeDescriptor, exp: Int) = (0 /: mkCubeH(cd).cubeById){
    case (acc, (_, cwo: CubeWithOrientation[SideName])) => acc + DistanceMeasure.moveDistance.apply(cwo)
  } === exp

}


class RubikCubeInstanceManualSpec
  extends RubikCubeManualSpec[RubikCubeInstance]("RubikCubeInstance")