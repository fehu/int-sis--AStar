Heuristics Used
===============

The heuristics are defiened in [RubikCube_A_*.scala](/rubik/src/main/scala/feh/tec/rubik/solve/RubikCube_A_*.scala).

**Target:** Minimize

### Solution Length
`RubikCube_A_*[T]#solutionLengthHeuristic`

The length of instance's parents chain. Not negative integer. This is *cost* heuristic.

### Distance Measure
`RubikCubeHeuristics.DistanceMeasure`

The shortest route (sequence of sides rotations) from current *sub-cube*'s pose to the correct one.

##### Distance Measure Calculation
**Concept:**

A *sub-cube* is uniquely defined by a set of sides colors, 
while it's *pose* can be defined by the same set, but zipped with the corresponding *orientations*.
Orientation is the Rubik's side (global axes, connected to the immobile sides' centers) 
faced by the given sub-cube's side at the moment. Orientation is uniquely defined by the same set as a sub-cube.

1. **Fix** some order for sub-cube's poses to adjust (current + expected).
2. **Foreach** possible sequence (order) of sides, **that** compose sub-cube's orientation, **do**:
  1. **Zip** the sequence of sides with the previously fixed poses.
  2. **Foreach** element 
     *(rotating side, sub-cube side's current orientation, sub-cube side's expected orientation)* of the resulting sequence, 
     **try** to calculate *rotating distance* for given sub-cube using only rotations of the provided *rotating side*:
    1. Using _*_, get the axis side that would be faced by the given sub-cube's side on the next rotation.
    2. **Case** it's euqal to the goal axis, **then** return the accumulated number of rotations.
    3. **Case** it's equal to the current one, **then** the rotation has no effect; return **Never**.
    4. **Case** the number of rotations tried exceeds 3, then the rotation has no effect; return **Never**.
    5. **Otherwise** go to *a*, incrementing the rotations accumulator.
  3. **Sum** the results, considering that `Never + ? == Never`.
  4. **Filter** out **Never** results.
3. **If** the resulting sequence isn't empty, select the **minimum**, **otherwise** return **Never**.

**Code:**

```scala
        protected def calcDistanceToGoal[T: WithSideName](k: CacheKey[T]): Measure = {
/* 1 */   val sidesSeq = k.from zip k.to
    
          val yieldRotations = Y[Seq[(SideName, (SideName, SideName))], Measure](
            rec => {
/* ii */      case Seq((rotSide, (from, to)), tail@ _*) => 
/* a, iii */    sideRotatingPartialDistance(from, to, rotSide) + rec(tail)
              case Nil => Measured(0)
            }
          )
    
          val routes: Seq[Int] = for{
/* 2 */     rotSeq  <- k.from.permutations.toStream
/* i, iv */ measure <- yieldRotations(rotSeq zip sidesSeq).toOption
            } yield measure
    
/* 3 */   if(routes.isEmpty) Never
          else Measured(routes.min)
        }

/* a */ protected def sideRotatingPartialDistance[T: WithSideName](from: SideName,
                                                                   to: SideName,
                                                                   rotating: SideName): Measure =
          Y[(SideName, Int), Measure](
            rec => {
              case (prev, c) =>
                val next = Rotation.next.byName(rotating)(prev)
/* b */         if      (prev == to)   Measured(c)
/* c */         else if (prev == next) Never
/* d */         else if (c > 3)        Never
/* e */         else                   rec(next, c+1)
              }
          )(from -> 0)
          

/* * */ Rotation.next{
          def byName(sideName: SideName) = sideName match {
            case Front => front
            case Back  => back
            case Right => right
            case Left  => left
            case Up    => up
            case Down  => down
          }
    
          lazy val front = next(Z, reverse = false)
          lazy val back  = next(Z, reverse = true)
          lazy val right = next(X, reverse = false)
          lazy val left  = next(X, reverse = true)
          lazy val up    = next(Y, reverse = false)
          lazy val down  = next(Y, reverse = true)
    
          lazy val X = Map(
            Front -> Up,
            Up    -> Back,
            Back  -> Down,
            Down  -> Front
          )
          lazy val Y = Map(
            Front -> Left,
            Left  -> Back,
            Back  -> Right,
            Right -> Front
          )
          lazy val Z = Map(
            Right -> Down,
            Down  -> Left,
            Left  -> Up,
            Up    -> Right
          )
    
          private def next(m: Map[SideName, SideName], reverse: Boolean) = {
            val mm = if (reverse) m.map(_.swap) else m
            mm.withDefault(identity)
          }
        }
```
##### Comment
The heuristic works under assumptions that a sub-cube may be moved to any valid position by rotations of only the sides that it's currently on. That yield maximum 2 sides to rotate for "crosses" and 3 for corners. Maximum 3 rotations per side. Heuristic's range (for a single sub-cube) is [0, 9].


