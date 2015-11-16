package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.util._

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

object RubikCube{

  def apply[T](sides: Map[SideName, Side[T]]): RubikCube[T] = RubikCube(
    sides(SideName.Front),
    sides(SideName.Right),
    sides(SideName.Left),
    sides(SideName.Up),
    sides(SideName.Down),
    sides(SideName.Back)
  )


  /** A smaller cube, that the Rubik's Cube is composed of. */
  sealed trait Cube[T] {
    def labels: Seq[T]
  }

  case class Center[T](name: SideName) extends Cube[T] { def labels = Nil }
  case class Middle[T](label1: T, label2: T) extends Cube[T]{ def labels = label1 :: label2 :: Nil }
  case class Corner[T](label1: T, label2: T, label3: T) extends Cube[T]{ def labels = label1 :: label2 :: label3 :: Nil }

//  object Orientation extends Enumeration{
//    val X, Y, Z = Value
//  }
//  type Orientation = Orientation.Value

  case class CubeSide[T](cube: Cube[T], label: T)



  lazy val side = 3

  /** Rubik's Cube side.
    *
    * @param cubes list of cube's rows
    * @tparam T label
    */
  case class Side[T](cubes: List[List[CubeSide[T]]]){
    side =>

    def center = cubes(1)(1).cube.asInstanceOf[Center[T]]
    object edge{
      def top    = mkEdge(cubes.head)
      def bottom = mkEdge(cubes(2))
      def right  = mkEdge(mkRight)
      def left   = mkEdge(mkLeft)

      def byName(name: EdgeName) = name match {
        case EdgeName.Right  => right
        case EdgeName.Left   => left
        case EdgeName.Top    => top
        case EdgeName.Bottom => bottom
      }
    }

    object setEdge{
      def top(l: SideEdge[T]) = copy(l.ensuring(_.side == side).edge :: cubes.tail)
      def bottom(l: SideEdge[T]) = copy(cubes.init :+ l.ensuring(_.side == side).edge)

      def byName(name: EdgeName) = name match {
        case EdgeName.Right  => ???
        case EdgeName.Left   => ???
        case EdgeName.Top    => top _
        case EdgeName.Bottom => bottom _
      }
    }

    /** Rotate the side 90 degrees clockwise */
    def rotate90 = Side(mkLeft :: mkMiddle :: mkRight :: Nil)

    /** Rotate the side 90 degrees counter-clockwise */
    def rotate_90 = Side(mkRight :: mkMiddle :: mkLeft :: Nil)

    def rotate180 = Side(cubes.reverse map (_.reverse))

    private def mkRight  = cubes.map(_(2))
    private def mkLeft   = cubes.map(_.head)
    private def mkMiddle = cubes.map(_(1))

    private def mkEdge = SideEdge(this, _: List[CubeSide[T]])
  }
  case class SideEdge[T](side: Side[T], edge: List[CubeSide[T]]){
    def left  = edge.head
    def right = edge(2)
    def middle = edge(1)
  }


  object SideName extends Enumeration{
    val Front, Right, Left, Up, Down, Back = Value
  }
  type SideName = SideName.Value

  object EdgeName extends Enumeration {
    val Left, Right, Top, Bottom = Value
  }
  type EdgeName = EdgeName.Value

/*
  implicit class EdgeNameOps(name: EdgeName){
    def opposite = name match {
      case EdgeName.Bottom => EdgeName.Top
      case EdgeName.Top    => EdgeName.Bottom
      case EdgeName.Left   => EdgeName.Right
      case EdgeName.Right  => EdgeName.Left
    }
  }
*/

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
  
  /*lazy val corners = Map(
    Side.Front -> Map(
      (Top, Left)     -> ((Side.Up,    (Bottom, Left)),    (Side.Left,  (Top, Right))),
      (Top, Right)    -> ((Side.Up,    (Bottom, Right)),   (Side.Right, (Top, Left))),
      (Bottom, Left)  -> ((Side.Down,  (Top, Left)),       (Side.Left,  (Bottom, Right))),
      (Bottom, Right) -> ((Side.Down,  (Top, Right)),      (Side.Right, (Bottom, Left)))
    ),
    Side.Right -> Map(
      (Top, Left)     -> ((Side.Up,    (Bottom, Right)),   (Side.Front, (Top, Right))),
      (Top, Right)    -> ((Side.Up,    (Top, Right)),      (Side.Back,  (Top, Left))),
      (Bottom, Left)  -> ((Side.Down,  (Top, Right)),      (Side.Front, (Bottom, Right))),
      (Bottom, Right) -> ((Side.Down,  (Bottom, Right)),   (Side.Back,  (Bottom, Left)))
    ),
    Side.Left -> Map(
      (Top, Left)     -> ((Side.Up,    (Top, Left)),       (Side.Back,  (Top, Right))),
      (Top, Right)    -> ((Side.Up,    (Bottom, Left)),    (Side.Front, (Top, Left))),
      (Bottom, Left)  -> ((Side.Down,  (Bottom, Left)),    (Side.Back,  (Bottom, Right))),
      (Bottom, Right) -> ((Side.Down,  (Top, Left)),       (Side.Front, (Bottom, Left)))
    ),
    Side.Up -> Map(
      (Top, Left)     -> ((Side.Back,  (Top, Right)),      (Side.Left,  (Top, Left))),
      (Top, Right)    -> ((Side.Back,  (Top, Left)),       (Side.Right, (Top, Right))),
      (Bottom, Left)  -> ((Side.Front, (Top, Left)),       (Side.Left,  (Top, Right))),
      (Bottom, Right) -> ((Side.Front, (Top, Right)),      (Side.Right, (Top, Left)))
    ),
    Side.Down -> Map(
      (Top, Left)     -> ((Side.Front, (Bottom, Left)),    (Side.Left,  (Bottom, Right))),
      (Top, Right)    -> ((Side.Front, (Bottom, Right)),   (Side.Right, (Bottom, Left))),
      (Bottom, Left)  -> ((Side.Back,  (Bottom, Right)),   (Side.Left,  (Bottom, Left))),
      (Bottom, Right) -> ((Side.Back,  (Bottom, Left)),    (Side.Right, (Bottom, Right)))
    ),
    Side.Back -> Map(
      (Top, Left)     -> ((Side.Up,    (Top, Right)),      (Side.Right, (Top, Right))),
      (Top, Right)    -> ((Side.Up,    (Top, Left)),       (Side.Left,  (Top, Left))),
      (Bottom, Left)  -> ((Side.Down,  (Bottom, Right)),   (Side.Right, (Bottom, Right))),
      (Bottom, Right) -> ((Side.Down,  (Bottom, Left)),    (Side.Left,  (Bottom, Left)))
    )
  )*/
  
//  lazy val middles = Map(
//    Side.Front -> Map(
//      Top -> ()
//    )
//  )
  
}



object RubikSubCubesDefault {

  lazy val centers: Set[Center[SideName]] = SideName.values.map(Center.apply[SideName])

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

    RubikCube(sides)
  }

  protected def atPos(s: Side[SideName], p: EdgeName): CubeSide[SideName] =  s.edge.byName(p).middle

  protected def atPos(s: Side[SideName], p: (EdgeName, EdgeName)): CubeSide[SideName] = {
    val row = p._1 match {
      case EdgeName.Top => s.edge.top
      case EdgeName.Bottom => s.edge.bottom
    }
    p._2 match {
      case EdgeName.Left => row.left
      case EdgeName.Right => row.right
    }
  }
}

object Test extends App{

  val r = RubikSubCubesDefault.randomCube()

  println(r)

}

/*
    def mkDependentSideCubes(c         : SideName,
                             mids      : Seq[Middle[SideName]],
                             neighbours: Map[EdgeName, Side[SideName]],
                             row       : Int = 1): (List[List[CubeSide[SideName]]], Seq[Middle[SideName]]) =
    {

      row match {
        case 1 =>
          val c1 = mkCorner(c, EdgeName.Left, EdgeName.Top, neighbours)
          val (m, newMids) = mkMiddle(EdgeName.Top, neighbours, mids)
          val c2 = mkCorner(c, EdgeName.Right, EdgeName.Top, neighbours)
          val crow = c1 :: m :: c2 :: Nil

          val (lst, mids_) = mkDependentSideCubes(c, newMids, neighbours, row + 1)
          (crow :: lst) -> mids_

        case 2 =>
          val (m1, newMids1) = mkMiddle(EdgeName.Left, neighbours, mids)
          val center = CubeSide(Center(c), c)
          val (m2, newMids2) = mkMiddle(EdgeName.Right, neighbours, newMids1)
          val crow = m1 :: center :: m2 :: Nil

          val (lst, mids_) = mkDependentSideCubes(c, newMids2, neighbours, row + 1)
          (crow :: lst) -> mids_

        case 3 =>
          val c1 = mkCorner(c, EdgeName.Left, EdgeName.Bottom, neighbours)
          val (m, newMids) = mkMiddle(EdgeName.Bottom, neighbours, mids)
          val c2 = mkCorner(c, EdgeName.Right, EdgeName.Bottom, neighbours)
          val crow = c1 :: m :: c2 :: Nil

          List(crow) -> mids
      }
    }

    def mkCorner(side: SideName, e1: EdgeName, e2: EdgeName, neighbours: Map[EdgeName, Side[SideName]]) = {
      def edgeOpt(e: EdgeName) = neighbours.get(e).map(_.edge.byName(e.opposite))

      def mk(c1Opt: Option[RubikCube.CubeSide[RubikCube.SideName]],
             c2Opt: Option[RubikCube.CubeSide[RubikCube.SideName]]) =
      {
        if (c1Opt.isEmpty   && c2Opt.isEmpty  ) ???

        val cl =
          if (c1Opt.isDefined && c2Opt.isDefined){
            val c1 = c1Opt.get
            val c2 = c2Opt.get
            assert(c1.cube == c2.cube)
            c1.cube -> chooseLabel(c1.cube.labels, Set(c1.label, c2.label), unique = true)
          }
          else {
            val c = c1Opt.orElse(c2Opt).get
            c.cube -> chooseLabel(c.cube.labels, Set(c.label))
          }
        (CubeSide.apply[SideName] _).tupled(cl)
      }

      e1 -> e2 match {

        case (EdgeName.Left, EdgeName.Top) | (EdgeName.Top, EdgeName.Left) =>
          val et = edgeOpt(EdgeName.Top)
          val el = edgeOpt(EdgeName.Left)
          side match {
            case SideName.Front => mk(et.map(_.left), el.map(_.right))
            case SideName.Right => mk(et.map(_.right), el.map(_.left))
            case SideName.Left  => mk(et.map(_.left), el.map(_.right))
            case SideName.Up    => mk(et.map(_.right), el.map(_.left))
            case SideName.Down  => mk(et.map(_.left), el.map(_.right))
            case SideName.Back  => mk(et.map(_.right), el.map(_.left))
          }

        case (EdgeName.Right, EdgeName.Top) | (EdgeName.Top, EdgeName.Right) =>
          val et = edgeOpt(EdgeName.Top)
          val er = edgeOpt(EdgeName.Right)
          
          side match {
            case SideName.Front | SideName.Left  | SideName.Down => mk(et.map(_.right), er.map(_.left))
            case SideName.Back  | SideName.Right | SideName.Up   => mk(et.map(_.left), er.map(_.left))
          }
          

        case (EdgeName.Left, EdgeName.Bottom) | (EdgeName.Bottom, EdgeName.Left) =>
          mk(edgeOpt(EdgeName.Bottom).map(_.left), edgeOpt(EdgeName.Left).map(_.right))

        case (EdgeName.Right, EdgeName.Bottom) | (EdgeName.Bottom, EdgeName.Right) =>
          mk(edgeOpt(EdgeName.Bottom).map(_.right), edgeOpt(EdgeName.Right).map(_.left))

      }

    }

    def mkMiddleOpt(e: EdgeName, neighbours: Map[EdgeName, Side[SideName]]) =
      neighbours.get(e).map(_.edge.byName(e.opposite)).map{
        edge =>
          val CubeSide(c, l) = edge.middle
          CubeSide(c, chooseLabel(c.labels, Set(l), unique = true))
      }

    def mkMiddle(e: EdgeName, neighbours: Map[EdgeName, Side[SideName]], free: Seq[Middle[SideName]]) =
      mkMiddleOpt(e, neighbours)
        .map(_ -> free)
        .getOrElse{
          val Some((m, newFree)) = free.randomPop
          CubeSide(m, m.labels.randomChoice.get) -> newFree
        }

    def chooseLabel(from: Seq[SideName], except: Set[SideName], unique: Boolean = false) ={
      val toSel = from.filterNot(except.contains)
      if(unique) toSel.ensuring(_.size == 1).head
      else toSel.randomChoice.get
    }


    val depFB_L = Map(EdgeName.Right -> front, EdgeName.Left -> back)
    val (lefts, mids1)  = mkDependentSideCubes(SideName.Left, rMiddles.drop(8), depFB_L)
    val left = Side(lefts)

    val depFB_R = Map(EdgeName.Left -> front, EdgeName.Right -> back)
    val (rights, mids2)  = mkDependentSideCubes(SideName.Right, mids1, depFB_R)
    val right = Side(rights)

    val delLR = Map(EdgeName.Left -> left, EdgeName.Right -> right)

    val depFBLR_U = delLR ++  Map(EdgeName.Bottom -> front, EdgeName.Top -> back)
    val (ups, mids3)  = mkDependentSideCubes(SideName.Up, mids2, depFBLR_U)
    val up = Side(ups)

    val depFBLR_D = delLR ++ Map(EdgeName.Bottom -> back, EdgeName.Top -> front)
    val (downs, mids4)  = mkDependentSideCubes(SideName.Down, mids3, depFBLR_D)
    val down = Side(downs)


    RubikCube(front, right, left, up, down, back)*/
    
