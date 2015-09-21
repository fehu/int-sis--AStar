package feh.tec.astar

import feh.tec.astar.SlidingPuzzleInstanceSpec._
import feh.tec.puzzles.SlidingPuzzle.GenericSlidingPuzzleInstance
import feh.tec.puzzles.{SlidingPuzzleInt3x3v1, SlidingPuzzle, SlidingPuzzleInstance}
import org.specs2._
import org.specs2.specification.core.Fragments
import feh.util._

import scala.language.{existentials, higherKinds}

/** An abstract test specification for [[SlidingPuzzleInstance]] implementations.
 */
trait SlidingPuzzleInstanceSpec[Impl[_] <: SlidingPuzzleInstance[_]] extends Specification{
  def implementationName: String

  protected def randomInstance: GetRandomInstance[Impl]

  def is = s2""" ${ s"Specification for `SlidingPuzzleInstance` implementation by `$implementationName`".name }

 A SlidingPuzzleInstance must:
   provide a piece at the given coordinate  $test_pieceAt
   provide a Map representation             $test_asMap
   provide the empty positions              $test_emptyPositions
   be able to move a piece                  $test_tryMove
   be immutable                             $test_immutable
   know the parent instance                 $test_parentInstance
   know its "generation"                    $test_generation
   equals another instance if and only if
     |1. has the same pieces configuration
     |2. reference the same puzzle          $test_equals
"""

  def test_pieceAt: Fragments = {
    val (inst, piecesMap) = randomInstance()
    forall(piecesMap){ case (coord, piece) => inst.pieceAt(coord) === piece }
  }

  def test_asMap: Fragments = {
    val (inst, piecesMap) = randomInstance()
    inst.asMap === piecesMap
  }

  def test_emptyPositions: Fragments = todo
  def test_tryMove: Fragments        = todo
  def test_immutable: Fragments      = todo
  def test_parentInstance: Fragments = todo
  def test_generation: Fragments     = todo
  def test_equals: Fragments         = todo
}

object SlidingPuzzleInstanceSpec{
  type GetRandomInstance[Impl[_]] =
    () => (Impl[Piece], Map[SlidingPuzzle.Coordinate, Option[Piece]]) forSome {type Piece}
}

abstract class GenericSlidingPuzzleInstanceSpec[Piece](val puzzle: SlidingPuzzle[Piece],
                                                       val randomInstance: GetRandomInstance[GenericSlidingPuzzleInstance])
  extends SlidingPuzzleInstanceSpec[GenericSlidingPuzzleInstance]
{
  def implementationName = "GenericSlidingPuzzleInstance"
}

class GenericSlidingPuzzleInstanceSpecExample1 extends GenericSlidingPuzzleInstanceSpec(
  GenericSlidingPuzzleInstanceSpecExample1.puzzle,
  GenericSlidingPuzzleInstanceSpecExample1.random
)

object GenericSlidingPuzzleInstanceSpecExample1{
  lazy val puzzle = new SlidingPuzzleInt3x3v1

  def random: GetRandomInstance[GenericSlidingPuzzleInstance] = () => {
    val rSeq = (1 to 9).randomOrder.map{
      case 9 => None
      case i => Some(i)
    }
    val rMp =
      for{
        y <- 0 until 3
        x <- 0 until 3
    } yield (x, y) -> rSeq(3*y + x)

    val rL = List(rSeq.slice(0, 3), rSeq.slice(3, 6), rSeq.slice(6, 9))

    val inst = new GenericSlidingPuzzleInstance[Int](puzzle, rL, None, 0)

    inst -> rMp.toMap
  }

}