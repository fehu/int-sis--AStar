package feh.tec.puzzles

import feh.util._

// is abstract to enforce a specific class name
abstract class GenericSlidingPuzzle[Piece](val width: Int,
                                           val height: Int,
                                           val emptySpaces: Int,
                                           solutionRows: List[List[Option[Piece]]]) extends SlidingPuzzle[Piece]
{
  lazy val solution = SlidingPuzzleInstance(this, solutionRows)

  def randomInstance = SlidingPuzzleInstance(this, solutionRows.randomOrder)
}


/** 3x3 Sliding Puzzle (perimeter version)
 * Solution:
 * | 1 | 2 | 3 |
 * | 8 |   | 4 |
 * | 7 | 6 | 5 |
 */
class SlidingPuzzleInt3x3v2 extends GenericSlidingPuzzle[Int](
  width = 3,
  height = 3,
  emptySpaces = 1,
  solutionRows = List(
    List(Some(1), Some(2), Some(3)),
    List(Some(8),  None  , Some(4)),
    List(Some(7), Some(6), Some(5))
  )
)
