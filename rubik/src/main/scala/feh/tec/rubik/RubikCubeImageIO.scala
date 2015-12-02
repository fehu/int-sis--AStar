package feh.tec.rubik

import java.text.DateFormat
import java.util.Date

import feh.tec.rubik.RubikCube.SideName
import feh.tec.rubik.RubikCubeImage.{SidesMap, Side}
import feh.util.{FileNameUtils, Path}
import feh.util.file._


trait RubikCubeImageIO {

  def readRaw: List[String] => RubikCubeImage[Int] = extractImage("RAW", _.toInt)

  def readColors: List[String] => RubikCubeImage[SideName] = extractImage("COLORS", SideName.fromString)

  protected def extractImage[R](chapter: String, f: String => R)(lines: List[String]): RubikCubeImage[R] = {
    val data = readImageLines(extractChapter(chapter, lines))
    RubikCubeImage(
      data.groupBy(_._1)
        .map{ case (side, l) => Side(l.map(p => p._2 -> f(p._3)).toMap, Some(side))}
        .toSeq
    )
  }

  type FullImage = RubikCubeImage[(Int, SideName)]

  def read(lines: List[String]): FullImage = readRaw(lines) merge readColors(lines)
  def read(file: File): FullImage = read( file.withInputStream(File.read[Seq[String]]).get.toList )

  def colorsOnly(file: File): RubikCubeImage[SideName] = readColors( file.withInputStream(File.read[Seq[String]]).get.toList )

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




  def write(imgs: Map[RubikCubeImage[_], ImageType], fileByTime: String => File, time: Date = new Date()): Unit = {
    val timeStr = DateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.FULL).format(time)
    val fStr = (new StringBuilder /: imgs){
      case (acc, (img, tpe@RawImage(sm))) => acc ++= s"\n$tpe\n" + RubikCubeImage.toString(img, sm)
      case (acc, (img, tpe))              => acc ++= s"\n$tpe\n" + RubikCubeImage.toString(img)
    }
    fileByTime(timeStr) withOutputStream File.write.utf8(timeStr + "\n" + fStr.mkString)
  }

  sealed trait ImageType
  case class RawImage(sm: SidesMap) extends ImageType{ override def toString = ":RAW" }
  case object ColorsImage           extends ImageType{ override def toString = ":COLORS" }

}

object RubikCubeImageIO extends RubikCubeImageIO