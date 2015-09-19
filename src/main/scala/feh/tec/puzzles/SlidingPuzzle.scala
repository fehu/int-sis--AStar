package feh.tec.puzzles

import feh.util._

import scala.util.Try

/** An abstract sliding puzzle.
 * @see <url>https://en.wikipedia.org/wiki/Sliding_puzzle</url>
 * @see <url>http://mypuzzle.org/sliding</url>
 */
trait SlidingPuzzle {
  type Piece

  def width: Int
  def height: Int

  def size = (width, height)


  /** `(x, y)` */
  type Coordinate = (Int, Int)

  implicit class CoordinateWrapper(c: Coordinate){
    def x = c._1
    def y = c._2
  }


  /** The number of lacking pieces (empty spaces) in the puzzle */
  def emptySpaces: Int

  def piecesCount = width * height - emptySpaces



  def solution: SlidingPuzzleInstance

  def randomInstance: SlidingPuzzleInstance
}

/**
 * Coordinates begin in left-upper corner, starting from 1.
 */
trait SlidingPuzzleInstance extends Equals{
  val puzzle: SlidingPuzzle

  import SlidingPuzzle._
  import puzzle._

  /**
   * @return Some(piece) if there is a piece at `c=(x, y)` or `None` if it's an empty space.
   */
  def pieceAt(c: Coordinate): Option[Piece]

  def listRows: Seq[Seq[Option[Piece]]]

  def emptyPositions: Seq[Coordinate]

  /** Try to move a piece `from` in the direction `dir`.
   *
   * @return `Success(copy_of_this_instance_with_move_applied)` or `Failure(...)`
   */
  def tryMove(from: Coordinate, dir: Direction): Try[SlidingPuzzleInstance]


  def parentInstance: Option[SlidingPuzzleInstance]

  /** Number of parents until root. */
  def generation: Int
}

object SlidingPuzzle{

  sealed trait Direction

  object Direction{
    case object North extends Direction
    case object East  extends Direction
    case object South extends Direction
    case object West  extends Direction
  }

  implicit class DirectionOps(dir: Direction){
    def opposite = dir match {
      case Direction.North => Direction.South
      case Direction.East  => Direction.West
      case Direction.South => Direction.North
      case Direction.West  => Direction.East
    }
  }

  implicit class SlidingPuzzleInstanceOps(val inst: SlidingPuzzleInstance){
    import inst.puzzle._

    def neighbouringPiecesCoordinates(c: Coordinate): Seq[(Direction, Coordinate)] = {
      val dirs = Direction.North :: Direction.East :: Direction.South :: Direction.West :: Nil
      dirs.zipMap(neighbourExistanceCheck(c)).collect{ case (dir, Some(cor)) => dir -> cor }
    }

    protected def neighbourExistanceCheck: Coordinate => Direction => Option[Coordinate] =
      coord =>
        dir =>
          PartialFunction.condOpt(dir){
            case Direction.North if coord.y != 1      => (coord.x, coord.y - 1)
            case Direction.East  if coord.x != width  => (coord.x + 1, coord.y)
            case Direction.South if coord.y != height => (coord.x, coord.y + 1)
            case Direction.West  if coord.x != 1      => (coord.x - 1, coord.y)
          }

  }

}