package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.RubikSubCubesDefault.WithSideNameIdentity

import scala.collection.mutable

/** Mutable Rubik's Cube */
class Rubik[T: WithSideName](initialCubes: Set[Cube[T]]) {
  import SideName._

  def cubes: Map[(Int, Int, Int), (Cube[T], CubeOrientation)] = Map(
    (0, 2, 2) -> corners(Set(Front, Left, Up)),
    (1, 2, 2) -> middles(Set(Front, Up)),
    (2, 2, 2) -> corners(Set(Front, Right, Up)),
    (0, 1, 2) -> middles(Set(Front, Left)),
    (1, 1, 2) -> centers(Set(Front)),
    (2, 1, 2) -> middles(Set(Front, Right)),
    (0, 0, 2) -> corners(Set(Front, Left, Down)),
    (1, 0, 2) -> middles(Set(Front, Down)),
    (2, 0, 2) -> corners(Set(Front, Right, Down)),

    (0, 2, 1) -> middles(Set(Up, Left)),
    (1, 2, 1) -> centers(Set(Up)),
    (2, 2, 1) -> middles(Set(Up, Right)),
    (0, 1, 1) -> centers(Set(Left)),
    (2, 1, 1) -> centers(Set(Right)),
    (0, 0, 1) -> middles(Set(Down, Left)),
    (1, 0, 1) -> centers(Set(Down)),
    (2, 0, 1) -> middles(Set(Down, Right)),

    (0, 2, 0) -> corners(Set(Back, Left, Up)),
    (1, 2, 0) -> middles(Set(Back, Up)),
    (2, 2, 0) -> corners(Set(Back, Right, Up)),
    (0, 1, 0) -> middles(Set(Back, Left)),
    (1, 1, 0) -> centers(Set(Back)),
    (2, 1, 0) -> middles(Set(Back, Right)),
    (0, 0, 0) -> corners(Set(Back, Left, Down)),
    (1, 0, 0) -> middles(Set(Back, Down)),
    (2, 0, 0) -> corners(Set(Back, Right, Down))
  )
  
  def sides: Map[SideName, Map[(Int, Int), (Cube[T], CubeOrientation)]] = Map(
    Front -> Map(
      (0, 2) -> corners(Set(Front, Left, Up)),
      (1, 2) -> middles(Set(Front, Up)),
      (2, 2) -> corners(Set(Front, Right, Up)),
      (0, 1) -> middles(Set(Front, Left)),
      (1, 1) -> centers(Set(Front)),
      (2, 1) -> middles(Set(Front, Right)),
      (0, 0) -> corners(Set(Front, Left, Down)),
      (1, 0) -> middles(Set(Front, Down)),
      (2, 0) -> corners(Set(Front, Right, Down))
    ),
    Right -> Map(
      (0, 2) -> corners(Set(Right, Front, Up)),
      (1, 2) -> middles(Set(Right, Up)),
      (2, 2) -> corners(Set(Right, Back, Up)),
      (0, 1) -> middles(Set(Right, Front)),
      (1, 1) -> centers(Set(Right)),
      (2, 1) -> middles(Set(Right, Back)),
      (0, 0) -> corners(Set(Right, Front, Down)),
      (1, 0) -> middles(Set(Right, Down)),
      (2, 0) -> corners(Set(Right, Back, Down))
    ),
    Left -> Map(
      (0, 2) -> corners(Set(Left, Back, Up)),
      (1, 2) -> middles(Set(Left, Up)),
      (2, 2) -> corners(Set(Left, Front, Up)),
      (0, 1) -> middles(Set(Left, Back)),
      (1, 1) -> centers(Set(Left)),
      (2, 1) -> middles(Set(Left, Front)),
      (0, 0) -> corners(Set(Left, Back, Down)),
      (1, 0) -> middles(Set(Left, Down)),
      (2, 0) -> corners(Set(Left, Front, Down))
    ),
    Up -> Map(
      (0, 2) -> corners(Set(Up, Left, Back)),
      (1, 2) -> middles(Set(Up, Back)),
      (2, 2) -> corners(Set(Up, Right, Back)),
      (0, 1) -> middles(Set(Up, Left)),
      (1, 1) -> centers(Set(Up)),
      (2, 1) -> middles(Set(Up, Right)),
      (0, 0) -> corners(Set(Up, Left, Front)),
      (1, 0) -> middles(Set(Up, Front)),
      (2, 0) -> corners(Set(Up, Right, Front))
    ),
    Down -> Map(
      (0, 2) -> corners(Set(Down, Left, Front)),
      (1, 2) -> middles(Set(Down, Front)),
      (2, 2) -> corners(Set(Down, Right, Front)),
      (0, 1) -> middles(Set(Down, Left)),
      (1, 1) -> centers(Set(Down)),
      (2, 1) -> middles(Set(Down, Right)),
      (0, 0) -> corners(Set(Down, Left, Back)),
      (1, 0) -> middles(Set(Down, Back)),
      (2, 0) -> corners(Set(Down, Right, Back))
    ),
    Back -> Map(
      (0, 2) -> corners(Set(Back, Right, Up)),
      (1, 2) -> middles(Set(Back, Up)),
      (2, 2) -> corners(Set(Back, Left, Up)),
      (0, 1) -> middles(Set(Back, Right)),
      (1, 1) -> centers(Set(Back)),
      (2, 1) -> middles(Set(Back, Left)),
      (0, 0) -> corners(Set(Back, Right, Down)),
      (1, 0) -> middles(Set(Back, Down)),
      (2, 0) -> corners(Set(Back, Left, Down))
    )
  )


  private def cubeId(c: Cube[T]) =  c.labels.map(_.side).toSet
  
  protected lazy val corners = mutable.HashMap(
    RubikSubCubes.corners.map{
      case ((s1, _), (s2, _), (s3, _)) =>
        val id = Set(s1, s2, s3)
        val c = initialCubes
          .collectFirst{ case c: Corner[T] if id == cubeId(c) => c }
          .get
        val o = CubeOrientation(s1, s2)

        id -> (c, o)
    }: _*
  )

  protected lazy val middles = mutable.HashMap(
    RubikSubCubes.middles.map{
      case ((s1, _), (s2, _)) =>
        val id = Set(s1, s2)
        val c = initialCubes
          .collectFirst{ case m: Middle[T] if id == cubeId(m) => m }
          .get
        val o = CubeOrientation(s1, s2)

        id -> (c, o)
    }: _*
  )

  protected lazy val centers = mutable.HashMap(
    SideName.values.toSeq.map{
      side => 
        val c = initialCubes
          .collectFirst{ case c@Center(l) if l.side == side => c }
          .get
        val o = CubeOrientation(side, side)
        
        Set(side) -> (c, o)
    }: _*)
}


case class CubeOrientation(o1: SideName, o2: SideName){
  import CubeOrientation._
  
  /** rotate Right Side clockwise 90 degrees */
  def rotateRight = CubeOrientation(nextRotRight.getOrElse(o1, o1), nextRotRight.getOrElse(o2, o2))

  /** rotate Left Side clockwise 90 degrees */
  def rotateLeft = CubeOrientation(nextRotLeft.getOrElse(o1, o1), nextRotLeft.getOrElse(o2, o2))

  /** rotate Front Side clockwise 90 degrees */
  def rotateFront = CubeOrientation(nextRotFront.getOrElse(o1, o1), nextRotFront.getOrElse(o2, o2))

  /** rotate Back Side clockwise 90 degrees */
  def rotateBack = CubeOrientation(nextRotBack.getOrElse(o1, o1), nextRotBack.getOrElse(o2, o2))

  /** rotate Up Side clockwise 90 degrees */
  def rotateUp = CubeOrientation(nextRotUp.getOrElse(o1, o1), nextRotUp.getOrElse(o2, o2))

  /** rotate Down Side clockwise 90 degrees */
  def rotateDown = CubeOrientation(nextRotDown.getOrElse(o1, o1), nextRotDown.getOrElse(o2, o2))
}

object CubeOrientation{
  import SideName._
  
  protected lazy val nextRotRight = Map(
    Front -> Up,
    Up    -> Back,
    Back  -> Down,
    Down  -> Front
  )
  protected lazy val nextRotLeft = nextRotRight.map(_.swap)

  protected lazy val nextRotFront = Map(
    Up    -> Right,
    Right -> Down,
    Down  -> Left,
    Left  -> Up
  )
  protected lazy val nextRotBack = nextRotFront.map(_.swap)

  protected lazy val nextRotUp = Map(
    Front -> Right,
    Right -> Back,
    Back  -> Left,
    Left  -> Front
  )
  protected lazy val nextRotDown = nextRotUp.map(_.swap)
}


object RubikTest extends App{

  val rubic = new Rubik[SideName](RubikSubCubesDefault.cubes)

  println(rubic.cubes)

}