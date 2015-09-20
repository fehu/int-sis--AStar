package feh.tec.puzzles.solve.run

import feh.tec.puzzles.SlidingPuzzleInt3x3v2

object SlidingPuzzle_Example1_v1 extends App{
  import feh.tec.puzzles.solve.SlidingPuzzle_A_*._

    val puzzle = new SlidingPuzzleInt3x3v2
    val initial = puzzle.randomInstance

    println(initial)

    val solver = Solve.solver_v1[Int]
    val res = solver.search(initial)

    println(res)

}
