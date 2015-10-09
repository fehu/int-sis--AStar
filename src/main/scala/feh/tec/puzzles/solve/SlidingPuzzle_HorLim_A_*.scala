package feh.tec.puzzles.solve

import feh.tec.astar.LimitedHorizon
import feh.tec.puzzles.SlidingPuzzleInstance


trait SlidingPuzzle_HorLim_A_*[Piece] extends SlidingPuzzle_A_*[Piece] with LimitedHorizon[SlidingPuzzleInstance[Piece]]

//object SlidingPuzzle_HorLim_A_*{
//
//  object SelectTheBest{
//
//    trait ValueThreshold[Piece] extends SlidingPuzzle_HorLim_A_*[Piece]{
//      def limHorThreshold: Heuristic
//
//      def selectTheBest = sps => sps.underlying.filterKeys(_ >= limHorThreshold).toMap.mapValues(_.toSet)
//    }
//
//    trait FractionOfBestThreshold[Piece] extends SlidingPuzzle_HorLim_A_*[Piece]{
//      def limHorFraction: InUnitInterval
//
//      implicit def heuristicIsNumeric: Numeric[Heuristic]
//
//      def limHorThreshold(bestH: Heuristic): Heuristic //  best*limHorFraction
//
//      def selectTheBest = sps => {
//        val best
//        sps.underlying.filterKeys(_ >= limHorThreshold).toMap.mapValues(_.toSet)
//      }
//    }
//
//  }
//
//}
