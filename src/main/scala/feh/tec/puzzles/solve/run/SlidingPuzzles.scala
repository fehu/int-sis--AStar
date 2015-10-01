package feh.tec.puzzles.solve.run

import feh.tec.astar.{VisualizeState, History, VisualizeHistory, HistoryEntry}
import feh.tec.puzzles.solve.SlidingPuzzle_A_*.Solve
import feh.tec.puzzles.{SlidingPuzzleInstance, SlidingPuzzleInt3x3v1, SlidingPuzzleInt3x3v2}
import feh.util._
import feh.tec.puzzles.SlidingPuzzle._

object SlidingPuzzle_Example1 extends App{
  import feh.tec.puzzles.solve.SlidingPuzzle_A_*._

  val puzzle = new SlidingPuzzleInt3x3v2
//  val initial = puzzle.randomInstance

  val initial = List(
    List(Some(2), Some(8), Some(3)),
    List(None   , Some(1), Some(4)),
    List(Some(7), Some(6), Some(5))
  ) |> (SlidingPuzzleInstance.initial(puzzle, _))

  println(initial)

  val solver = Solve.solver_v1[Int]

  val res = solver.search(initial)

//  println(res)

  res._1.map{
    _.pathFromRoot.zipWithIndex.foreach{ case (inst, i) => println(s"\t$i:\t $inst") }
  } getOrElse println(res)

  println("History:")
  res._2.get.foreach{
    case HistoryEntry(parent, children) => println(s"$parent\t===>\t ${children.mkString(", ")}")
  }

  val vh = new VisualizeHistory[SlidingPuzzleInstance[Int]]{
    lazy val drawState: VisualizeState[SlidingPuzzleInstance[Int]] = null

    protected def heuristic = solver.heuristic
    protected def depthOf = _.generation.toInt
    protected def description = _.description

    def drawHistory(h: History[SlidingPuzzleInstance[Int]]) = ???
  }

  lazy val aTree = vh.abstractHistoryTree(res._2)

  println()
  aTree.Debug.listTree()

}

object SlidingPuzzle_Example2 extends App{
  val puzzle = new SlidingPuzzleInt3x3v1

//  val initial = puzzle.randomInstance

  val initial = List(
    List(Some(8), Some(7), None),
    List(Some(5), Some(2), Some(6)),
    List(Some(4), Some(3), Some(1))
  )|> (SlidingPuzzleInstance.initial(puzzle, _))

  val solver = Solve.solver_v1[Int]

  val res = solver.search(initial)

  println(res)
}