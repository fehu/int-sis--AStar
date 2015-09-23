package feh.tec.astar

import feh.tec.astar.SlidingPuzzleInstanceSpec.InstanceData
import feh.tec.puzzles.SlidingPuzzle.{Coordinate, Direction, GenericSlidingPuzzleInstance}
import feh.tec.puzzles.{SlidingPuzzleInt3x3v1, SlidingPuzzle, SlidingPuzzleInstance}
import org.scalacheck.{Prop, Gen, Arbitrary}
import org.specs2._
import org.specs2.specification.core.Fragments
import feh.util._

import scala.language.{existentials, higherKinds}
import scala.util.{Failure, Success, Try}


// TODO: Use ScalaCheck

/** An abstract test specification for [[SlidingPuzzleInstance]] implementations.
 */
trait SlidingPuzzleInstanceSpec[Impl[_] <: SlidingPuzzleInstance[_]] extends Specification with ScalaCheck{
  def implementationName: String

  implicit def randomInstanceData: Arbitrary[InstanceData]

  def instanceFromData: InstanceData => Impl[_]

  implicit def randomInstance: Arbitrary[Impl[_]] = Arbitrary{
    implicitly[Arbitrary[InstanceData]].arbitrary.map(instanceFromData)
  }

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
     1. has the same pieces configuration
     2. reference the same puzzle           $test_equals
"""

  def test_pieceAt = prop{
    data: InstanceData =>
      val inst = instanceFromData(data)
      forall(data){ case (coord, piece) => inst.pieceAt(coord) === piece }
  }

  def test_asMap = prop{ data: InstanceData => instanceFromData(data).asMap === data }

  def test_emptyPositions = prop{
    data: InstanceData =>
      val empties = data.filter(_._2.isEmpty).keySet
      instanceFromData(data).emptyPositions.toSet === empties
  }

  def test_tryMove = prop{
    inst: Impl[_] =>
      forall(inst.emptyPositions){
        case eCor@(ex, ey) =>
          forall(Direction.all){
            dir =>
              val pieceCor = dirFrom(dir, eCor)
              inst.tryMove(pieceCor, dir.opposite) match {
                case Success(newInst) =>
                  val tParams = (inst, newInst, eCor, pieceCor)
                  (notMovedTest _).tupled(tParams) && (movedTest _).tupled(tParams)
                case _: Failure[_] =>
                  val isOut = outOfBoardTest(pieceCor, inst).toResult.isSuccess ||
                              outOfBoardTest(eCor, inst).toResult.isSuccess
                  lazy val outOfBoard = outOfBoardTest(pieceCor, inst) :| "origin point is out of the board" ||
                                        outOfBoardTest(eCor, inst)     :| "destination point is out of the board"
                  lazy val incorrectPieces = (inst.pieceAt(pieceCor) must beNone) :| "tried to move empty space" ||
                                             (inst.pieceAt(eCor) must beSome)     :| "tried to move into a piece"

                  outOfBoard.iff{
                    case _ if isOut => Prop.passed
                    case prop       => incorrectPieces
                  }
              }
          }
      }
  }

  protected def outOfBoardTest(cor: Coordinate, inst: Impl[_]) = List(
    cor._1 must beLessThan(0),
    cor._1 must beGreaterThanOrEqualTo(inst.puzzle.width),
    cor._2 must beLessThan(0),
    cor._2 must beGreaterThanOrEqualTo(inst.puzzle.height)
  ).reduce(_ or _)

  protected def notMovedTest(initial: SlidingPuzzleInstance[_],
                             moved: SlidingPuzzleInstance[_], 
                             movedTo: Coordinate, 
                             movedFrom: Coordinate) =
    forall(moved.asMap -- Set(movedFrom, movedTo)){
      case (c, v) => initial.pieceAt(c) mustEqual v
    } :| "every field, except `origin` and `destination` should remain unchanged"

  protected def movedTest(initial: SlidingPuzzleInstance[_],
                          moved: SlidingPuzzleInstance[_],
                          movedTo: Coordinate,
                          movedFrom: Coordinate) =
    (initial.pieceAt(movedTo) must beNone)                        :| "the destination was initially empty" &&
    (moved.pieceAt(movedTo) must beSome)                          :| "a piece has been set at the destination" &&
    (moved.pieceAt(movedTo) mustEqual initial.pieceAt(movedFrom)) :| "the destination has been changed correctly" &&
    (moved.pieceAt(movedFrom) must beNone)                        :| "the origin has been set empty"

  private def dirFrom(dir: Direction, from: Coordinate): Coordinate = from match {
    case (x, y) => dir match {
      case Direction.North => (x, y-1)
      case Direction.East  => (x+1, y)
      case Direction.South => (x, y+1)
      case Direction.West  => (x-1, y)
    }
  }
  

  def test_immutable: Fragments      = todo
  def test_parentInstance: Fragments = todo
  def test_generation: Fragments     = todo
  def test_equals: Fragments         = todo
}

object SlidingPuzzleInstanceSpec{
  type InstanceData = Map[SlidingPuzzle.Coordinate, Option[_]]
}

abstract class GenericSlidingPuzzleInstanceSpec[Piece](val puzzle: SlidingPuzzle[Piece],
                                                       val randomInstanceData: Arbitrary[InstanceData],
                                                       val instanceFromData: InstanceData => GenericSlidingPuzzleInstance[Piece] )
  extends SlidingPuzzleInstanceSpec[GenericSlidingPuzzleInstance]
{
  def implementationName = "GenericSlidingPuzzleInstance"
}

class GenericSlidingPuzzleInstanceSpecExample1 extends GenericSlidingPuzzleInstanceSpec[Int](
  GenericSlidingPuzzleInstanceSpecExample1.puzzle,
  GenericSlidingPuzzleInstanceSpecExample1.randomData,
  GenericSlidingPuzzleInstanceSpecExample1.mkInstance
)


object GenericSlidingPuzzleInstanceSpecExample1{
  lazy val puzzle = new SlidingPuzzleInt3x3v1

  def randomData: Arbitrary[InstanceData] = Arbitrary{
    val pieces = None +: (1 to 8).map(Option(_)) |> (_.randomOrder)
    val genOrder = Gen.listOfN(9, Gen.posNum[Int]).suchThat(l => l.distinct == l)
    val shuffledPieces = genOrder.map(_.zip(pieces).sortBy(_._1).map(_._2))
    shuffledPieces.map{
      shPieces =>
        (0 until puzzle.height).flatMap{
          y =>
            val row = shPieces.slice(y * puzzle.width, (y + 1) * puzzle.width)
            row.zipWithIndex.map{ case (v, x) => (x, y) -> v }
        }.toMap
    }
  }

  def mkInstance: InstanceData => GenericSlidingPuzzleInstance[Int] = {
    data =>
      val rows = data.groupBy(_._1._2).mapValues(_.toList.sortBy(_._1._1).map(_._2)).toList.sortBy(_._1).map(_._2)
      new GenericSlidingPuzzleInstance[Int](puzzle, rows.asInstanceOf[Seq[Seq[Option[Int]]]], None, 0)
  }

}
