package feh.tec.puzzles.solve

import feh.tec.astar.A_*
import feh.tec.puzzles.SlidingPuzzle._
import feh.tec.puzzles.SlidingPuzzleInstance

import scala.util.Success

/** Implements `transformations` for [[SlidingPuzzleInstance]] */
trait SlidingPuzzle_A_*[Piece] extends A_*[SlidingPuzzleInstance[Piece]]{

  /** Lists the next possible states. */
  def transformations =
    inst => for { empty    <- inst.emptyPositions
                  (dir, c) <- inst.neighbouringPiecesCoordinates(empty)
                  Success(moved) = inst.tryMove(c, dir.opposite)
          } yield moved

  def isSolution = inst => inst == inst.puzzle.solution


}
