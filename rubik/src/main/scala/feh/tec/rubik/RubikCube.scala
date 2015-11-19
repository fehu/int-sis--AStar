package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.util._

import scala.collection.mutable

trait RubikCube[T]{
  type ThisType <: RubikCube[T]

  def cubeById: Map[CubeId, CubeWithOrientation[T]]

  /** rotate a side 90 degrees clockwise */
  def rotate(sideName: SideName): ThisType


  def cubes: Map[(Int, Int, Int), CubeWithOrientation[T]] = cubeById.mapKeys(RubikCube.cubePosition)

  def sides(sideName: SideName): Map[(Int, Int), CubeWithOrientation[T]] =
    RubikCube.sideCubes(sideName).mapValues(cubeById)

  /** rotate a side 90 degrees clockwise */
  protected def rotateUpdate(sideName: SideName) =
    for ( (pos, (c, o)) <- sides(sideName) )
      yield Update(c, o.rotate(sideName), MutableRubikCube.rotationPosChange(sideName, pos))


  protected case class Update(put: Cube[T], o: CubeOrientation, at: CubeId)

  def snapshot: RubikCubeInstance[T]
}


object RubikCube{

  type CubeId = Set[SideName]

  type CubeWithOrientation[T] = (Cube[T], CubeOrientation)

  trait WithSideName[T] { def side: T => SideName }

  implicit class WithSideNameWrapper[T: WithSideName](t: T){
    def side = implicitly[WithSideName[T]].side(t)
  }

  /** A smaller cube, that the Rubik's Cube is composed of. */
  sealed trait Cube[T] {
    def labels: Seq[T]
  }

  case class Center[T: WithSideName](label: T) extends Cube[T] { def labels = label :: Nil }
  case class Middle[T: WithSideName](label1: T, label2: T) extends Cube[T]{ def labels = label1 :: label2 :: Nil }
  case class Corner[T: WithSideName](label1: T, label2: T, label3: T) extends Cube[T]{ def labels = label1 :: label2 :: label3 :: Nil }


  case class CubeOrientation(o1: SideName, o2: SideName, o3: SideName){

    def rotate(r: SideName) = {
      val rot = NextRotation.byName(r)
      CubeOrientation(rot(o1), rot(o2), rot(o3))
    }

    def toSeq = Seq(o1, o2, o3)
  }


  object SideName extends Enumeration{
    val Front, Right, Left, Up, Down, Back = Value
  }
  type SideName = SideName.Value

  object EdgeName extends Enumeration {
    val Left, Right, Top, Bottom = Value
  }
  type EdgeName = EdgeName.Value


  lazy val cubePosition = cubeAt.map(_.swap)
  lazy val cubeAt: Map[(Int, Int, Int), CubeId] = {
    import SideName._
    Map(
      (0, 2, 2) -> Set(Front, Left, Up),
      (1, 2, 2) -> Set(Front, Up),
      (2, 2, 2) -> Set(Front, Right, Up),
      (0, 1, 2) -> Set(Front, Left),
      (1, 1, 2) -> Set(Front),
      (2, 1, 2) -> Set(Front, Right),
      (0, 0, 2) -> Set(Front, Left, Down),
      (1, 0, 2) -> Set(Front, Down),
      (2, 0, 2) -> Set(Front, Right, Down),

      (0, 2, 1) -> Set(Up, Left),
      (1, 2, 1) -> Set(Up),
      (2, 2, 1) -> Set(Up, Right),
      (0, 1, 1) -> Set(Left),
      (2, 1, 1) -> Set(Right),
      (0, 0, 1) -> Set(Down, Left),
      (1, 0, 1) -> Set(Down),
      (2, 0, 1) -> Set(Down, Right),

      (0, 2, 0) -> Set(Back, Left, Up),
      (1, 2, 0) -> Set(Back, Up),
      (2, 2, 0) -> Set(Back, Right, Up),
      (0, 1, 0) -> Set(Back, Left),
      (1, 1, 0) -> Set(Back),
      (2, 1, 0) -> Set(Back, Right),
      (0, 0, 0) -> Set(Back, Left, Down),
      (1, 0, 0) -> Set(Back, Down),
      (2, 0, 0) -> Set(Back, Right, Down)
    )
  }

  lazy val sidePositions = sideCubes.mapValues(_.map(_.swap))

  lazy val sideCubes: Map[SideName, Map[(Int, Int), CubeId]] = {
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

    object NextRotation{
      import SideName._

      val rotationMapCache = mutable.HashMap.empty[(Int, Boolean), Map[SideName, SideName]]

      def byName(sideName: SideName) = sideName match {
        case Front => front
        case Back  => back
        case Right => right
        case Left  => left
        case Up    => up
        case Down  => down
      }

      lazy val front = next(Z, reverse = false)
      lazy val back  = next(Z, reverse = true)
      lazy val right = next(X, reverse = false)
      lazy val left  = next(X, reverse = true)
      lazy val up    = next(Y, reverse = false)
      lazy val down  = next(Y, reverse = true)

      lazy val X = Map(
        Front -> Up,
        Up    -> Back,
        Back  -> Down,
        Down  -> Front
      )
      lazy val Y = Map(
        Front -> Left,
        Left  -> Back,
        Back  -> Right,
        Right -> Front
      )
      lazy val Z = Map(
        Right -> Down,
        Down  -> Left,
        Left  -> Up,
        Up    -> Right
      )

      private def next(m: Map[SideName, SideName], reverse: Boolean) = {
        val mm = if (reverse) m.map(_.swap) else m
        mm.withDefault(identity)
      }
    }
}


object RubikSubCubes{
  import EdgeName._

  def cornersOf(sideName: SideName, p: (EdgeName, EdgeName)) =
    corners.find{ _.productIterator.contains(sideName -> p) }
      .orElse( corners.find{ _.productIterator.contains(sideName -> p.swap) } )
      .get

  def middleOf(sideName: SideName, p: EdgeName) =
    middles.find{ _.productIterator.contains(sideName -> p) }.get


  private def Side = SideName

  lazy val corners = List(
    (Side.Front -> (Top, Left),     Side.Up   -> (Bottom, Left),  Side.Left  -> (Top, Right)),
    (Side.Front -> (Top, Right),    Side.Up   -> (Bottom, Right), Side.Right -> (Top, Left)),
    (Side.Front -> (Bottom, Right), Side.Down -> (Top, Right),    Side.Right -> (Bottom, Left)),
    (Side.Front -> (Bottom, Left),  Side.Down -> (Top, Left),     Side.Left  -> (Bottom, Right)),
    (Side.Right -> (Top, Right),    Side.Up   -> (Top, Right),    Side.Back  -> (Top, Left)),
    (Side.Left  -> (Top, Left),     Side.Up   -> (Top, Left),     Side.Back  -> (Top, Right)),
    (Side.Down  -> (Bottom, Left),  Side.Back -> (Bottom, Right), Side.Left  -> (Bottom, Left)),
    (Side.Down  -> (Bottom, Right), Side.Back -> (Bottom, Left),  Side.Right -> (Bottom, Right))
  )

  lazy val middles = List(
    (Side.Front, Top)    -> (Side.Up,    Bottom),
    (Side.Front, Right)  -> (Side.Right, Left),
    (Side.Right, Top)    -> (Side.Up,    Right),
    (Side.Right, Right)  -> (Side.Back,  Left),
    (Side.Left,  Top)    -> (Side.Up,    Left),
    (Side.Left,  Right)  -> (Side.Front, Left),
    (Side.Up,    Top)    -> (Side.Back,  Top),
    (Side.Down,  Top)    -> (Side.Front, Bottom),
    (Side.Down,  Right)  -> (Side.Right, Bottom),
    (Side.Back,  Right)  -> (Side.Left,  Left),
    (Side.Back,  Bottom) -> (Side.Down,  Bottom),
    (Side.Left,  Bottom) -> (Side.Down,  Left)
  )

}


object RubikSubCubesDefault {

  def cubes = ( RubikSubCubesDefault.centers ++ RubikSubCubesDefault.middles ++ RubikSubCubesDefault.corners
    ).asInstanceOf[Set[Cube[SideName]]]

  lazy val centers: Set[Center[SideName]] = SideName.values.map(Center.apply[SideName])

  implicit object WithSideNameIdentity extends WithSideName[SideName]{ def side = identity }

  lazy val middles = {
    val YZ = yz
    def mkWith(s: SideName) = YZ.map(Middle(s, _))
    val withFront = mkWith(SideName.Front)
    val withBack = mkWith(SideName.Back)
    val rest = for {s1 <- y; s2 <- z} yield Middle(s1, s2)

    withFront ++ withBack ++ rest
  }

  lazy val corners = for {s1 <- x; s2 <- y; s3 <- z} yield Corner(s1, s2, s3)

  private def x = Set(SideName.Front, SideName.Back)

  private def y = Set(SideName.Left, SideName.Right)

  private def z = Set(SideName.Up, SideName.Down)

  private def yz = y ++ z

}
