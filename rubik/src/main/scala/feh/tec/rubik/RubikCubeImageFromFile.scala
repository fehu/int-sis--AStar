package feh.tec.rubik

import feh.tec.rubik.RubikCube.SideName
import feh.tec.rubik.RubikCubeImage.Side
import feh.util.file._


trait RubikCubeImageFromFile {

  def raw: List[String] => RubikCubeImage[Int] = extractImage("RAW", _.toInt)

  def colors: List[String] => RubikCubeImage[SideName] = extractImage("COLORS", SideName.fromString)

  def extractImage[R](chapter: String, f: String => R)(lines: List[String]): RubikCubeImage[R] = {
    val data = readImageLines(extractChapter(chapter, lines))
    RubikCubeImage(
      data.groupBy(_._1)
        .map{ case (side, l) => Side(l.map(p => p._2 -> f(p._3)).toMap, Some(side))}
        .toSeq
    )
  }

  type FullImage = RubikCubeImage[(Int, SideName)]

  def apply(lines: List[String]): FullImage = raw(lines) merge colors(lines)
  def apply(file: File): FullImage = apply( file.withInputStream(File.read[Seq[String]]).get.toList )

  def colorsOnly(file: File): RubikCubeImage[SideName] = colors( file.withInputStream(File.read[Seq[String]]).get.toList )

  protected def extractChapter(chapter: String, lines: List[String]) = lines
    .dropWhile(s => !s.startsWith(":" + chapter))
    .tail
    .takeWhile(s => !s.startsWith(":"))

  protected def readImageLines(lines: List[String]): List[(SideName, (Int, Int), String)] = readImageLines(lines, None)

  protected def readImageLines(lines: List[String], side: Option[SideName]): List[(SideName, (Int, Int), String)] =
    lines match {
      case h :: t if h.trim.isEmpty => readImageLines(t, side)
      case h :: t if h startsWith "-- " =>
        val side = SideName.fromString(h.drop(3).trim)
        readImageLines(t, Some(side))
      case h :: t if side.isDefined => h.split(',') match {
        case Array(x, y, v) => (side.get, x.trim.toInt -> y.trim.toInt,  v.trim) :: readImageLines(t, side)
      }
      case Nil => Nil
    }
}

object RubikCubeImageFromFile extends RubikCubeImageFromFile