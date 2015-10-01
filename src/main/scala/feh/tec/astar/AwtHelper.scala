package feh.tec.astar

import java.awt.{Dimension, Point}

import scala.language.implicitConversions

trait AwtHelper {
  implicit def intPairIsPoint(p: (Int, Int)): Point = new Point(p._1, p._2)
  implicit def intPairIsDimension(p: (Int, Int)): Dimension = new Dimension(p._1, p._2)
}
