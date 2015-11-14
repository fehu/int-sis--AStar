package feh.tec.puzzles.solve

import feh.tec.astar.A_*
import feh.tec.puzzles.SlidingPuzzle._
import feh.tec.puzzles.SlidingPuzzleInstance

import scala.util.Success

/** Implements `transformations` for [[SlidingPuzzleInstance]]
  */
trait SlidingPuzzle_A_*[Piece] extends A_*[SlidingPuzzleInstance[Piece]]{

  /** Lists the next possible states. */
  def transformations =
    inst => for { empty    <- inst.emptyPositions
                  (dir, c) <- inst.neighbouringPiecesCoordinates(empty)
                  Success(moved) = inst.tryMove(c, dir.opposite)
          } yield moved

  def isSolution = inst => inst == inst.puzzle.solution

  def description = _.description

  protected def listParents = _.pathFromRoot
}

object SlidingPuzzle_A_*{

  /** An A* algorithm for [[feh.tec.puzzles.SlidingPuzzle]], minimizing the given heuristic
    */
  class MinimizingHeuristic[Piece, H](val heuristic: SlidingPuzzleInstance[Piece] => H)
                                     (implicit val heuristicOrdering: Ordering[H])
    extends SlidingPuzzle_A_*[Piece]
    with A_*.MinimizingHeuristic[SlidingPuzzleInstance[Piece]]
  {
    type Heuristic = H
  }

  /** An A* algorithm for [[feh.tec.puzzles.SlidingPuzzle]], maximizing the given heuristic
    */
  class MaximizingHeuristic[Piece, H](val heuristic: SlidingPuzzleInstance[Piece] => H)
                                     (implicit val heuristicOrdering: Ordering[H])
    extends SlidingPuzzle_A_*[Piece]
    with A_*.MinimizingHeuristic[SlidingPuzzleInstance[Piece]]
  {
    type Heuristic = H
  }

  /** Some heuristics for [[SlidingPuzzleInstance]]s. */
  object Heuristics{

    object Double{

      /** The sum of manhattan distances from the piece to it's correct position. */
      def manhattanDistanceToSolution[Piece]: SlidingPuzzleInstance[Piece] => (Coordinate, Piece) => Double =
        inst =>
          (c, piece) => {
            val (dx, dy) = inst.puzzle.SolutionRapidAccess.piecePositions(piece) - c
            dx.abs + dy.abs
          }

      /** The number of actions taken to arrive to this state (state's parents).  */
      def solutionLength: SlidingPuzzleInstance[_] => Double = _.generation

      /** The number of correctly positioned pieces. */
      def correctlySet: SlidingPuzzleInstance[_] => Double =
        inst => (
          for {
            (c, iv) <- inst.asMap
            sv = inst.puzzle.solution.asMap(c)
            if sv == iv
          } yield 1
        ).sum

      /** The number of correctly set rows and columns. */
      def correctRowsAndCols: SlidingPuzzleInstance[_] => Double =
        inst => correctRows(inst) + correctCols(inst)

      /** The number of correctly set rows. */
      def correctRows: SlidingPuzzleInstance[_] => Double =
        inst => correctRowsInner(inst.listRows, inst.puzzle.solution.listRows)

      /** The number of correctly set columns. */
      def correctCols: SlidingPuzzleInstance[_] => Double =
        inst => correctRowsInner(inst.listRows.transpose, inst.puzzle.solution.listRows.transpose)

      protected def correctRowsInner(rows: Seq[Seq[Option[Any]]], srows: Seq[Seq[Option[Any]]]) =
        rows.zip(srows).count(==)

      object HasSingleEmpty{

        /** The sum of manhattan distances from each piece to it's correct position. */
        def manhattanDistanceToSolutionSum[Piece]: SlidingPuzzleInstance[Piece] => Double =
          inst => {
            // distance for pieces
            val dPieces = for { (coord, Some(piece)) <- inst.asMap }
              yield manhattanDistanceToSolution(inst)(coord, piece)
            // distance for empties
            val empties = inst.emptyPositions
            val empty = empties.ensuring(_.size == 1, "HasSingleEmpty supports one! empty: " + empties).head
            val deltaEmptyPos = inst.puzzle.solution.emptyPositions.head - empty
            val dEmpty = deltaEmptyPos._1.abs + deltaEmptyPos._2.abs

            dPieces.sum + dEmpty
          }

      }

    }

  }

  object Solve{
    def minimizing[Piece, H: Ordering](heristic: SlidingPuzzleInstance[Piece] => H) =
      new MinimizingHeuristic[Piece, H](heristic)
  }

}