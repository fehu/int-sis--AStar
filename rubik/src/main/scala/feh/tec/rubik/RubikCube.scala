package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.ogl.Utils.HalfPiMultAngle

import scala.collection.mutable

trait RubikCube[T]{
  def cubes: Map[(Int, Int, Int), CubeWithOrientation[T]]
}

/*
/** Rubik's Cube instance. */
case class RubikCube[T](front: RubikCube.Side[T],
                        right: RubikCube.Side[T],
                        left : RubikCube.Side[T],
                        up   : RubikCube.Side[T],
                        down : RubikCube.Side[T],
                        back : RubikCube.Side[T]
                         )
{
  def cubes: Seq[RubikCube.Side[T]] = productIterator.toSeq.map(_.asInstanceOf[RubikCube.Side[T]])

  def side(name: SideName) = name match {
    case SideName.Front => front
    case SideName.Right => right
    case SideName.Left  => left
    case SideName.Up    => up
    case SideName.Down  => down
    case SideName.Back  => back
  }


  protected val rotationEffects = Map(
    SideName.Front -> (
      front,
      List(
        up    -> EdgeName.Bottom,
        down  -> EdgeName.Top,
        left  -> EdgeName.Right,
        right -> EdgeName.Left
        )
      )
  )

  /** Rotate a side 90 degrees clockwise */
  def rotate90 = (rotateSide90 _).tupled compose rotationEffects


  protected type RotateSetup = List[(Side[T], EdgeName)]

  protected def rotateSide90(side: Side[T], affected: RotateSetup) = {
    val newSide = side.rotate90

    type Arg = (RotateSetup, SideEdge[T])

    val aLast = affected.last
     val aEdge = aLast._1.edge.byName(aLast._2)

    val rotated = Y[Arg, List[Side[T]]](
      rec => {
        case ((s, edge) :: Nil, acc)  => s.setEdge.byName(edge)(acc) :: Nil
        case ((s, edge) :: tail, acc) =>
          val read = s.edge.byName(edge)
          s.setEdge.byName(edge)(acc) :: rec(tail -> read)
      }
    )(affected -> aEdge)
  }

}
*/

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

      //      Right -> Map(
      //        (0, 2) -> Set(Right, Back, Up),
      //        (1, 2) -> Set(Right, Up),
      //        (2, 2) -> Set(Right, Front, Up),
      //        (0, 1) -> Set(Right, Back),
      //        (1, 1) -> Set(Right),
      //        (2, 1) -> Set(Right, Front),
      //        (0, 0) -> Set(Right, Back, Down),
      //        (1, 0) -> Set(Right, Down),
      //        (2, 0) -> Set(Right, Front, Down)),
      //      Left -> Map(
      //        (0, 2) -> Set(Left, Front, Up),
      //        (1, 2) -> Set(Left, Up),
      //        (2, 2) -> Set(Left, Back, Up),
      //        (0, 1) -> Set(Left, Front),
      //        (1, 1) -> Set(Left),
      //        (2, 1) -> Set(Left, Back),
      //        (0, 0) -> Set(Left, Front, Down),
      //        (1, 0) -> Set(Left, Down),
      //        (2, 0) -> Set(Left, Back, Down)),

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


/*
  def randomCube() = {
    val rCorners = corners.toSeq.randomOrder
    val rMiddles = middles.toSeq.randomOrder

    val (corFront, corBack) = rCorners.splitAt(4)
    val (midFront, midBack) = rMiddles.take(8).splitAt(4)

    def mkSideCubes(c: Center[SideName],
                    cors: Seq[Corner[SideName]],
                    mids: Seq[Middle[SideName]],
                    row: Int = 1): List[List[Cube[SideName]]] = {
      def mk2C = cors(0) :: mids(0) :: cors(1) :: Nil
      def mkM = mids(0) :: c :: mids(1) :: Nil

      row match {
        case 1 => mk2C :: mkSideCubes(c, cors.drop(2), mids.tail, row + 1)
        case 2 => mkM :: mkSideCubes(c, cors, mids.drop(2), row + 1)
        case 3 => mk2C :: Nil
      }
    }

    def chooseSide(c: Cube[SideName]): CubeSide[SideName] = c match {
      case Center(nme) => CubeSide(c, nme)
      case m => CubeSide(c, c.labels.randomChoice.get)
    }

    val front = Side(mkSideCubes(Center(SideName.Front), corFront, midFront).map(_.map(chooseSide)))
    val back = Side(mkSideCubes(Center(SideName.Back), corBack, midBack).map(_.map(chooseSide)))

    def mkDependentSideCubes(c: SideName,
                             mids: Seq[Middle[SideName]],
                             neighbours: Map[SideName, Side[SideName]],
                             row: Int = 1): (List[List[CubeSide[SideName]]], Seq[Middle[SideName]]) =
      row match {
        case 1 =>
          val c1 = mkCorner(c, EdgeName.Left, EdgeName.Top, neighbours)
          val (m, newMids) = mkMiddle(c, EdgeName.Top, neighbours, mids)
          val c2 = mkCorner(c, EdgeName.Right, EdgeName.Top, neighbours)
          val crow = c1 :: m :: c2 :: Nil

          val (lst, mids_) = mkDependentSideCubes(c, newMids, neighbours, row + 1)
          (crow :: lst) -> mids_

        case 2 =>
          val (m1, newMids1) = mkMiddle(c, EdgeName.Left, neighbours, mids)
          val center = CubeSide(Center(c), c)
          val (m2, newMids2) = mkMiddle(c, EdgeName.Right, neighbours, newMids1)
          val crow = m1 :: center :: m2 :: Nil

          val (lst, mids_) = mkDependentSideCubes(c, newMids2, neighbours, row + 1)
          (crow :: lst) -> mids_

        case 3 =>
          val c1 = mkCorner(c, EdgeName.Left, EdgeName.Bottom, neighbours)
          val (m, newMids) = mkMiddle(c, EdgeName.Bottom, neighbours, mids)
          val c2 = mkCorner(c, EdgeName.Right, EdgeName.Bottom, neighbours)
          val crow = c1 :: m :: c2 :: Nil

          List(crow) -> mids
      }

    def mkCorner(side: SideName, e1: EdgeName, e2: EdgeName, neighbours: Map[SideName, Side[SideName]]) = {
      val corners = RubikSubCubes.cornersOf(side, (e1, e2))
      val used = corners.productIterator
        .map(_.asInstanceOf[(SideName, (EdgeName, EdgeName))])
        .withFilter(_ != side ->(e1, e2))
        .withFilter(neighbours contains _._1)
        .map { case (name, pos) => atPos(neighbours(name), pos) }
        .toSeq

      if (used.isEmpty) ???

      val toSel = used.head.cube.labels.toSet -- used.map(_.label).toSet

      assert(toSel.nonEmpty && toSel.size < 3)

      val label = if (used.size == 2) {
        assert(used.head.cube == used(1).cube)
        toSel.ensuring(_.size == 1).head
      }
      else toSel.toSeq.randomChoice.get

      CubeSide(used.head.cube, label)
    }

    def mkMiddle(side: SideName, e: EdgeName, neighbours: Map[SideName, Side[SideName]], free: Seq[Middle[SideName]]) = {
      val (m1, m2) = RubikSubCubes.middleOf(side, e)
      val m = if (m1 == side -> e) m2 else m1
      val usedOpt = neighbours.get(m._1).map(atPos(_, e))

      val (cube, label, newFree) = usedOpt
        .map { case CubeSide(cube, l) => (cube, cube.labels.filter(_ != l).ensuring(_.size == 1).head, free) }
        .getOrElse {
          (free.head, free.head.labels.randomChoice.get, free.tail)
        }

      CubeSide(cube, label) -> newFree
    }


    val (_, sides) = (SideName.values -- Set(SideName.Front, SideName.Back)) // todo: _ => Nil
      .foldLeft(rMiddles.drop(8) -> Map(SideName.Front -> front, SideName.Back -> back)) {
        case ((mids, neigh), side) =>
          val (cubes, newMids) = mkDependentSideCubes(side, mids, neigh)
          val newNeigh = neigh + (side -> Side(cubes))
          newMids -> newNeigh
      }

//    RubikCube(sides)
    ???
  }
*/

/*  protected def atPos(s: Side[SideName], p: EdgeName): CubeSide[SideName] =  s.edge.byName(p).middle

  protected def atPos(s: Side[SideName], p: (EdgeName, EdgeName)): CubeSide[SideName] = {
    val row = p._1 match {
      case EdgeName.Top => s.edge.top
      case EdgeName.Bottom => s.edge.bottom
    }
    p._2 match {
      case EdgeName.Left => row.left
      case EdgeName.Right => row.right
    }
  }*/
}
