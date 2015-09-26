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

  object Heuristics{

    object Double{

      def manhattanDistanceToSolution[Piece]: SlidingPuzzleInstance[Piece] => (Coordinate, Piece) => Double =
        inst =>
          (c, piece) => {
            val (dx, dy) = inst.puzzle.SolutionRapidAccess.piecePositions(piece) - c
            dx.abs + dy.abs
          }


      def solutionLength: SlidingPuzzleInstance[_] => Double = _.generation

      object HasSingleEmpty{

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

        def correctRows[Piece]: SlidingPuzzleInstance[Piece] => Double = ???
        def correctCols[Piece]: SlidingPuzzleInstance[Piece] => Double = ???

      }

    }

  }

  object Solve{
    /** Solves a [[feh.tec.puzzles.SlidingPuzzle]] using [[MinimizingHeuristic]]
     *  where heuristic = f - g
     *        f = [[Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum]]
     *        g = [[Heuristics.Double.solutionLength]]
     */
    def solver_v1[Piece]: SlidingPuzzle_A_*[Piece] = {
      val f = Heuristics.Double.HasSingleEmpty.manhattanDistanceToSolutionSum[Piece]
      val g = Heuristics.Double.solutionLength
      val heristic = (x: SlidingPuzzleInstance[Piece]) => f(x) + g(x)

      new MinimizingHeuristic[Piece, Double](heristic)
    }

    //(initial: SlidingPuzzleInstance[Piece])
    //A_*[SlidingPuzzleInstance[Piece]]#Result
//    val res = a_*.search(initial)
  }

}