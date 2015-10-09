package feh.tec.puzzles.solve

import feh.tec.astar.A_*.SortedPossibilities
import feh.tec.astar.{History, LimitedHorizon}
import feh.tec.puzzles.SlidingPuzzleInstance
import feh.util.RecFunc

import scala.util.Try


trait SlidingPuzzle_HorLim_A_*[Piece] extends SlidingPuzzle_A_*[Piece] with LimitedHorizon[SlidingPuzzleInstance[Piece]]

object SlidingPuzzle_HorLim_A_*{
  trait RENAME_ME[Piece] extends SlidingPuzzle_HorLim_A_*[Piece]{
    def selectTheBest = ???
  }
}
