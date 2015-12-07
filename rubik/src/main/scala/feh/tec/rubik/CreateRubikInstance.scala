package feh.tec.rubik

import feh.tec.rubik.RubikCube._
import feh.tec.rubik.RubikCubeImage.{ColorMap, SidesMap}
import feh.util._

object CreateRubikInstance {

  def fromRaw[T, C](sidesMap: Map[SideName, Map[(Int, Int), T]],
                    parent: Option[RubikCubeInstance[C]],
                    description: RubikCube.Description)
                   (implicit cm: ColorMap[T, C], wsn: WithSideName[C]): RubikCubeInstance[C] =
    apply(sidesMap.mapValues(_.mapValues(cm.colorFor)), parent, description)

  def apply[T: WithSideName](sidesMap: Map[SideName, Map[(Int, Int), T]],
                             parent: Option[RubikCubeInstance[T]],
                             description: RubikCube.Description): RubikCubeInstance[T] =
  {
    val idsO = sidesMap.flatMap{
      case (side, tMap) =>
        val idsMap = sideCubes(side)
        tMap.mapKeys(idsMap andThen (_ -> side))
    }
    val grouped = RubikCubeImage groupCubes idsO.groupBy(_._1).values.flatMap(_.map{
      case ((id, side), x) => id -> (side, x)
    })

    fromCubes(grouped, parent, description)
  }

  def fromCubes[T: WithSideName](cubes: Map[CubeId, CubeWithOrientation[T]],
                                 parent: Option[RubikCubeInstance[T]],
                                 description: RubikCube.Description): RubikCubeInstance[T] =
    RubikCubeInstance(cubes, parent, description)


  def apply[T: WithSideName](img: RubikCubeImage[T],
                             parent: Option[RubikCubeInstance[T]],
                             description: RubikCube.Description)
                            (implicit sMap: SidesMap): RubikCubeInstance[T] =
    RubikCubeInstance(RubikCubeImage.readCubesWithMap(img), parent, description)

  def fromSnapshot[T: WithSideName](img: RubikCubeImage[T],
                                    parent: Option[RubikCubeInstance[T]],
                                    description: RubikCube.Description): RubikCubeInstance[T] =
    RubikCubeInstance(RubikCubeImage.readCubes(img), parent, description)
}