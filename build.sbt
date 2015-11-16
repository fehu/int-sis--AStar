name := "A*-root"

publishArtifact := false

CommonSettings()



lazy val root = project in file(".") aggregate (aStar, slidingPuzzle, rubik)


lazy val aStar          = project in file("a-star")

lazy val slidingPuzzle  = project in file("sliding-puzzle") dependsOn aStar

lazy val rubik          = project in file("rubik") dependsOn aStar