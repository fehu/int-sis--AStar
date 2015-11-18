package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.RubikSubCubesDefault.WithSideNameIdentity
import feh.tec.rubik.ogl.Utils.{Angle0, HalfPiMultAngle}

import scala.collection.immutable.Iterable
import scala.collection.mutable

/** Mutable Rubik's Cube */
class Rubik[T: WithSideName](initialCubes: Set[Cube[T]]) {
  import SideName._

  def cubes: Map[(Int, Int, Int), (Cube[T], CubeOrientation)] = Map(
    (0, 0, 2) -> corners(Set(Front, Left, Up)),
    (0, 1, 2) -> middles(Set(Front, Up)),
    (0, 2, 2) -> corners(Set(Front, Right, Up)),
    (1, 0, 2) -> middles(Set(Front, Left)),
    (1, 1, 2) -> centers(Set(Front)),
    (1, 2, 2) -> middles(Set(Front, Right)),
    (2, 0, 2) -> corners(Set(Front, Left, Down)),
    (2, 1, 2) -> middles(Set(Front, Down)),
    (2, 2, 2) -> corners(Set(Front, Right, Down)),

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

  def sides: Map[SideName, Map[(Int, Int), (Cube[T], CubeOrientation)]] =
    Rubik.sideCubes.mapValues(_.mapValues(cubesHeap))
  

  /** rotate a side 90 degrees clockwise */
  def rotate(sideName: SideName): Unit = bulkUpdate{
    for ( (pos, (c, o)) <- sides(sideName) )
      yield Update(c, o.rotate(sideName), Rubik.rotationPosChange(sideName, pos))
  }
  
  case class Update(put: Cube[T], o: CubeOrientation, at: Set[SideName])
  
  def bulkUpdate(upds: Iterable[Update]): Unit = {
    upds.foreach{ case Update(c, o, at) => putInCube(at, c, o) }
  }
  
  private def putInCube(at: Set[SideName], c: Cube[T], o: CubeOrientation) = at.size match {
    case 1 => centers += at -> (c.asInstanceOf[Center[T]], o)
    case 2 => middles += at -> (c.asInstanceOf[Middle[T]], o)
    case 3 => corners += at -> (c.asInstanceOf[Corner[T]], o)
  }
  

  private def cubeId(c: Cube[T]) =  c.labels.map(_.side).toSet

  def cubesHeap = corners ++ middles ++ centers
  
  
  protected def defaultOrientation = CubeOrientation(Angle0, Angle0, Angle0)
  
  protected lazy val corners = mutable.HashMap(
    RubikSubCubes.corners.map{
      case ((s1, _), (s2, _), (s3, _)) =>
        val id = Set(s1, s2, s3)
        val c = initialCubes
          .collectFirst{ case c: Corner[T] if id == cubeId(c) => c }
          .get
        val o = defaultOrientation

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

        id -> (c, defaultOrientation)
    }: _*
  )

  protected lazy val centers = mutable.HashMap(
    SideName.values.toSeq.map{
      side => 
        val c = initialCubes
          .collectFirst{ case c@Center(l) if l.side == side => c }
          .get
        
        Set(side) -> (c, defaultOrientation)
    }: _*)
}

object Rubik{

//  def sideAngle(side: SideName) = side match {
//    case
//  }

  def orientationByPos(pos: Set[SideName]) = pos.size match {
    case 3 =>
  }
  
  def rotationPosChange(side: SideName, pos: (Int, Int)): Set[SideName] =
    sideCubes(side)(rotationIntPosChange(pos))

  def rotationPosChange(side: SideName, pos: Set[SideName]): Set[SideName] =
    rotationPosChange(side, sidePositions(side)(pos))
  
  lazy val rotationIntPosChange = Map(
    (0, 2) -> (2, 2),
    (1, 2) -> (2, 1),
    (2, 2) -> (2, 0),
    (2, 1) -> (1, 0),
    (2, 0) -> (0, 0),
    (1, 0) -> (0, 1),
    (0, 0) -> (0, 2),
    (0, 1) -> (1, 2),
    (1, 1) -> (1, 1)
  )

  lazy val sidePositions = sideCubes.mapValues(_.map(_.swap))
  
  lazy val sideCubes: Map[SideName, Map[(Int, Int), Set[SideName]]] = {
    import SideName._
    Map(
      Front -> Map(
        (0, 2) -> Set(Front, Left, Up),
        (1, 2) -> Set(Front, Up),
        (2, 2) -> Set(Front, Right, Up),
      
        (0, 1) -> Set(Front, Left),
        (1, 1) -> Set(Front),
        (2, 1) -> Set(Front, Right),
      
        (0, 0) -> Set(Front, Left, Down),
        (1, 0) -> Set(Front, Down),
        (2, 0) -> Set(Front, Right, Down)),
    
      Right -> Map(
        (0, 2) -> Set(Right, Front, Up),
        (1, 2) -> Set(Right, Up),
        (2, 2) -> Set(Right, Back, Up),
        (0, 1) -> Set(Right, Front),
        (1, 1) -> Set(Right),
        (2, 1) -> Set(Right, Back),
        (0, 0) -> Set(Right, Front, Down),
        (1, 0) -> Set(Right, Down),
        (2, 0) -> Set(Right, Back, Down)),
      Left -> Map(
        (0, 2) -> Set(Left, Back, Up),
        (1, 2) -> Set(Left, Up),
        (2, 2) -> Set(Left, Front, Up),
        (0, 1) -> Set(Left, Back),
        (1, 1) -> Set(Left),
        (2, 1) -> Set(Left, Front),
        (0, 0) -> Set(Left, Back, Down),
        (1, 0) -> Set(Left, Down),
        (2, 0) -> Set(Left, Front, Down)),
      Up -> Map(
        (0, 2) -> Set(Up, Left, Back),
        (1, 2) -> Set(Up, Back),
        (2, 2) -> Set(Up, Right, Back),
        (0, 1) -> Set(Up, Left),
        (1, 1) -> Set(Up),
        (2, 1) -> Set(Up, Right),
        (0, 0) -> Set(Up, Left, Front),
        (1, 0) -> Set(Up, Front),
        (2, 0) -> Set(Up, Right, Front)),
      Down -> Map(
        (0, 2) -> Set(Down, Left, Front),
        (1, 2) -> Set(Down, Front),
        (2, 2) -> Set(Down, Right, Front),
        (0, 1) -> Set(Down, Left),
        (1, 1) -> Set(Down),
        (2, 1) -> Set(Down, Right),
        (0, 0) -> Set(Down, Left, Back),
        (1, 0) -> Set(Down, Back),
        (2, 0) -> Set(Down, Right, Back)),
      Back -> Map(
        (0, 2) -> Set(Back, Right, Up),
        (1, 2) -> Set(Back, Up),
        (2, 2) -> Set(Back, Left, Up),
        (0, 1) -> Set(Back, Right),
        (1, 1) -> Set(Back),
        (2, 1) -> Set(Back, Left),
        (0, 0) -> Set(Back, Right, Down),
        (1, 0) -> Set(Back, Down),
        (2, 0) -> Set(Back, Left, Down)
      )
    )
  }

}


case class CubeOrientation(ax: HalfPiMultAngle, ay: HalfPiMultAngle, az: HalfPiMultAngle){

  /** rotate Right Side clockwise 90 degrees */
  def rotateRight = CubeOrientation(ax, ay.plus, az)

  /** rotate Left Side clockwise 90 degrees */
  def rotateLeft = CubeOrientation(ax, ay.minus, az)

  /** rotate Front Side clockwise 90 degrees */
  def rotateFront = CubeOrientation(ax.plus, ay.plus, az)

  /** rotate Back Side clockwise 90 degrees */
  def rotateBack = CubeOrientation(ax.minus, ay.minus, az)

  /** rotate Up Side clockwise 90 degrees */
  def rotateUp = CubeOrientation(ax.plus, ay, az)

  /** rotate Down Side clockwise 90 degrees */
  def rotateDown = CubeOrientation(ax.minus, ay, az)

  def rotate(r: SideName) = r match {
    case SideName.Front => rotateFront
    case SideName.Back  => rotateBack
    case SideName.Left  => rotateLeft
    case SideName.Right => rotateRight
    case SideName.Up    => rotateUp
    case SideName.Down  => rotateDown
  }
}

object CubeOrientation{
  import SideName._

  @deprecated
  protected lazy val nextRotRight = Map(
    Front -> Up,
    Up    -> Back,
    Back  -> Down,
    Down  -> Front
  )
  @deprecated
  protected lazy val nextRotLeft = nextRotRight.map(_.swap)

  @deprecated
  protected lazy val nextRotFront = Map(
    Up    -> Right,
    Right -> Down,
    Down  -> Left,
    Left  -> Up
  )
  @deprecated
  protected lazy val nextRotBack = nextRotFront.map(_.swap)

  @deprecated
  protected lazy val nextRotUp = Map(
    Front -> Right,
    Right -> Back,
    Back  -> Left,
    Left  -> Front
  )
  @deprecated
  protected lazy val nextRotDown = nextRotUp.map(_.swap)
}


object RubikTest extends App{

  val rubic = new Rubik[SideName](RubikSubCubesDefault.cubes)

  println(rubic.cubes)

}