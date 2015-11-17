name := "A*-rubic"

version := "0.1-SNAPSHOT"

CommonSettings()

TestSettings()

libraryDependencies += "org.lwjgl.lwjgl" % "lwjgl" % "2.8.4"

javaOptions in Runtime += "-Djava.library.path=" +
                          (baseDirectory.value / "native") // "lwjgl-platform-2.9.0-natives-linux.jar"

