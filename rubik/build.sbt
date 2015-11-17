name := "A*-rubic"

version := "0.2-SNAPSHOT"

CommonSettings()

TestSettings()

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at
    "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at
    "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "com.storm-enroute" %% "macrogl" % "0.4-SNAPSHOT")

LWJGLPlugin.lwjglSettings