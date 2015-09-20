package feh.tec.puzzles

import feh.tec.puzzles.SlidingPuzzle._
import feh.util._

import scala.util.Try

/** An abstract sliding puzzle.
 * @see <url>https://en.wikipedia.org/wiki/Sliding_puzzle</url>
 * @see <url>http://mypuzzle.org/sliding</url>
 */
trait SlidingPuzzle[Piece] {

  def width: Int
  def height: Int

  def size = (width, height)

  /** The number of lacking pieces (empty spaces) in the puzzle */
  def emptySpaces: Int

  def piecesCount = width * height - emptySpaces



  def solution: SlidingPuzzleInstance[Piece]

  def randomInstance: SlidingPuzzleInstance[Piece]


  trait SolutionRapidAccess {
    /** The opposite of `asMap`, but contains only pieces.
     */
    def piecePositions: Map[Piece, Coordinate]
  }

  val SolutionRapidAccess: SolutionRapidAccess = new SolutionRapidAccess{
    lazy val piecePositions = solution.asMap.collect{
      case (coord, Some(piece)) => piece -> coord
    }
  }
}

/**
 * Coordinates begin in left-upper corner, starting from 0.
 */
trait SlidingPuzzleInstance[Piece]{

  val puzzle: SlidingPuzzle[Piece]

  /**
   * @return Some(piece) if there is a piece at `c=(x, y)` or `None` if it's an empty space.
   */
  def pieceAt(c: Coordinate): Option[Piece]

  def listRows: Seq[Seq[Option[Piece]]]

  def asMap: Map[Coordinate, Option[Piece]]

  def emptyPositions: Seq[Coordinate]

  /** Try to move a piece `from` in the direction `dir`.
   *
   * @return `Success(copy_of_this_instance_with_move_applied)` or `Failure(...)`
   */
  def tryMove(from: Coordinate, dir: Direction): Try[SlidingPuzzleInstance[Piece]]


  def parentInstance: Option[SlidingPuzzleInstance[Piece]]

  /** Number of parents until root. */
  def generation: Long

  override def equals(obj: scala.Any) = obj match {
    case that: SlidingPuzzleInstance[_] => that.listRows == this.listRows
  }

  override def hashCode() = listRows.hashCode()
}

object SlidingPuzzleInstance{
  def apply[Piece](puzzle: SlidingPuzzle[Piece],
                   listRows: Seq[Seq[Option[Piece]]]): SlidingPuzzleInstance[Piece] =
    new GenericSlidingPuzzleInstance(puzzle, listRows, None, 0)
}

object SlidingPuzzle{

  /** `(x, y)` */
  type Coordinate = (Int, Int)

  implicit class CoordinateWrapper(c: Coordinate){
    def x = c._1
    def y = c._2

    def +(c2: Coordinate) = (c.x + c2.x, c.y + c2.y)
    def -(c2: Coordinate) = (c.x - c2.x, c.y - c2.y)
  }

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

  implicit class SlidingPuzzleInstanceOps[Piece](val inst: SlidingPuzzleInstance[Piece]){
    def neighbouringPiecesCoordinates(c: Coordinate): Seq[(Direction, Coordinate)] = {
      val dirs = Direction.North :: Direction.East :: Direction.South :: Direction.West :: Nil
      dirs.zipMap(neighbourRelatively(inst.puzzle)(c)).collect{ case (dir, Some(cor)) => dir -> cor }
    }

  }

  def neighbourRelatively: SlidingPuzzle[_] => Coordinate => Direction => Option[Coordinate] =
    puzzle =>
      coord =>
        dir =>
          PartialFunction.condOpt(dir){
            case Direction.North if coord.y != 0                => (coord.x, coord.y - 1)
            case Direction.East  if coord.x != puzzle.width -1  => (coord.x + 1, coord.y)
            case Direction.South if coord.y != puzzle.height -1 => (coord.x, coord.y + 1)
            case Direction.West  if coord.x != 0                => (coord.x - 1, coord.y)
          }

  /** Immutable puzzle state. */
  class GenericSlidingPuzzleInstance[Piece](val puzzle: SlidingPuzzle[Piece],
                                            val listRows: Seq[Seq[Option[Piece]]],
                                            val parentInstance: Option[SlidingPuzzleInstance[Piece]],
                                            val generation: Long )
    extends SlidingPuzzleInstance[Piece]
  {
    /**
     * @return Some(piece) if there is a piece at `c=(x, y)` or `None` if it's an empty space.
     */
    def pieceAt(c: (Int, Int)) = listRows(c.y)(c.x)

    /** Try to move a piece `from` in the direction `dir`.
      *
      * @return `Success(copy_of_this_instance_with_move_applied)` or `Failure(...)`
      */
    def tryMove(from: (Int, Int), dir: Direction) = Try{
      val Some(piece) = pieceAt(from)
      val Some(to)    = neighbourRelatively(puzzle)(from)(dir)
      assert(pieceAt(to).isEmpty, "Cannot move to " + to)

      val newRows = updateListRows(Map( from -> None,
                                        to   -> Some(piece)
                                      ))
      new GenericSlidingPuzzleInstance(puzzle, newRows, Some(this), generation + 1)
    }

    lazy val asMap = listRows.zipWithIndex.flatMap{
      case (row, y) =>
        row.zipWithIndex.map{
          case (v, x) => (x, y) -> v
        }
    }.toMap

    lazy val emptyPositions = for{ (row, y) <- listRows.zipWithIndex
                                   (None,x) <- row.zipWithIndex
                            } yield x -> y


    protected def updateListRows(upds: Map[Coordinate, Option[Piece]]) = {
      val rowsToUpdate = upds.keySet.map(_.y)

      listRows.zipWithIndex.map{
          case (row, y) if rowsToUpdate contains y =>
            row.zipWithIndex.map{
              case (piece, x) => upds.getOrElse(x -> y, piece)
            }
          case (row, _) => row
        }
    }

    override def toString = listRows.toString()
  }
  
}

