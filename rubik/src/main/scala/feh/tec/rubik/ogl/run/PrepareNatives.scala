package feh.tec.rubik.ogl.run

import feh.util.Path._
import feh.util.file._

import scala.util.Success

object PrepareNatives {
  def defineOs = sys.props("os.name").toLowerCase.take(3).toString match {
    case "lin" => "linux"
    case "mac" => "osx"
    case "win" => "windows"
    case _ => sys.error("unknown OS")
  }

  def defineArch = sys.props
    .get("os.arch")
    .filter(_.contains("64"))
    .map(_ => 64)
    .getOrElse(32)

  private object Filename{
    def linux = Seq("liblwjgl", "libopenal")
  }

  // this can change any moment
  def nativeFiles = (defineOs, defineArch) match {
    case ("linux",   32) => Filename.linux.map(_ + ".so")
    case ("linux",   64) => Filename.linux.map(_ + "64.so")
    case ("windows", 32) => Seq("lwjgl.dll", "OpenAL32.dll")
    case ("windows", 64) => Seq("lwjgl64.dll", "OpenAL64.dll")
    case ("osx",      _) => Seq("liblwjgl.jnilib", "openal.dylib")
  }



  def nativesPath = / / "lwjgl-resources" / defineOs

  def setLwjglLibraryPath(file: File) = sys.props("org.lwjgl.librarypath") = file.ensuring(_.canRead)
                                                                                 .ensuring(_.isDirectory)
                                                                                 .getAbsolutePath

  def copyNativesToDir(dir: FileUtils#FileBuilder) = {

    for {
      fName <- nativeFiles
      fSrc = nativesPath / fName
      x = fSrc.mkString("/")
      src = getClass.getClassLoader.getResourceAsStream(x)
      Success(file) = dir.createFile(fName)
    } {

//      println("fSrc = " + x)
      file.withOutputStream(File.write.large(src)).get
    }
  }

  def withTempDir[R](f: FileUtils#FileBuilder => R): R = {
    val tmp = File.temporaryDir("resources.lwjgl")
//    println("with temp dir " + tmp.dirPath)
    try f(tmp)
    finally {
      tmp.dir.listFiles().foreach(_.delete())
      tmp.dir.delete().ensuring(identity[Boolean] _, "failed to cleanup temp files")
    }
  }

  def initNatives(fb: FileUtils#FileBuilder) = {
    copyNativesToDir(fb)
    setLwjglLibraryPath(fb.dir)
  }
  
  def andThen[R](f: => R): R = withTempDir{
    dir =>
      initNatives(dir)
      println("copied resources to " + dir.dirPath.mkString("/"))
      f
  }
}
