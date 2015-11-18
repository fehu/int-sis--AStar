package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.util._


object RubikCube{


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



  object SideName extends Enumeration{
    val Front, Right, Left, Up, Down, Back = Value
  }
  type SideName = SideName.Value

  object EdgeName extends Enumeration {
    val Left, Right, Top, Bottom = Value
  }
  type EdgeName = EdgeName.Value

}




@deprecated("remove it")
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

