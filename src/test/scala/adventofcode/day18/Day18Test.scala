package adventofcode.day18

import adventofcode.AoCTest
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class Day18Test extends AoCTest {
  override val lines: String = """2,2,2
                                 |1,2,2
                                 |3,2,2
                                 |2,1,2
                                 |2,3,2
                                 |2,2,1
                                 |2,2,3
                                 |2,2,4
                                 |2,2,6
                                 |1,2,5
                                 |3,2,5
                                 |2,1,5
                                 |2,3,5""".stripMargin
  val cubes: Set[Coordinates3D] = Coordinates3D.fromLines(input)
  test("parse") {
    assertEquals(
      Coordinates3D.fromLines(input),
      Set(
        Coordinates3D(2, 2, 2),
        Coordinates3D(1, 2, 2),
        Coordinates3D(3, 2, 2),
        Coordinates3D(2, 1, 2),
        Coordinates3D(2, 3, 2),
        Coordinates3D(2, 2, 1),
        Coordinates3D(2, 2, 3),
        Coordinates3D(2, 2, 4),
        Coordinates3D(2, 2, 6),
        Coordinates3D(1, 2, 5),
        Coordinates3D(3, 2, 5),
        Coordinates3D(2, 1, 5),
        Coordinates3D(2, 3, 5)
      )
    )
  }
  test("adjacent cubes") {
    assertEquals(Coordinates3D(1, 1, 1).isAdjacent(Coordinates3D(2, 1, 1)), true)
  }
  property("adjacent is commutative") {
    forAll { (x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int) =>
      val cube1 = Coordinates3D(x1, y1, z1)
      val cube2 = Coordinates3D(x2, y2, z2)
      cube1.isAdjacent(cube2) == cube2.isAdjacent(cube1)
    }
  }
  test("visible sides") {
    assertEquals(Coordinates3D(2, 2, 2).visibleSides(cubes), 0)
    assertEquals(Coordinates3D(1, 2, 2).visibleSides(cubes), 5)
    assertEquals(Coordinates3D(3, 2, 2).visibleSides(cubes), 5)
    assertEquals(Coordinates3D(2, 1, 2).visibleSides(cubes), 5)
    assertEquals(Coordinates3D(2, 3, 2).visibleSides(cubes), 5)
    assertEquals(Coordinates3D(2, 2, 1).visibleSides(cubes), 5)
    assertEquals(Coordinates3D(2, 2, 3).visibleSides(cubes), 4)
    assertEquals(Coordinates3D(2, 2, 4).visibleSides(cubes), 5)
    assertEquals(Coordinates3D(2, 2, 6).visibleSides(cubes), 6)
    assertEquals(Coordinates3D(1, 2, 5).visibleSides(cubes), 6)
    assertEquals(Coordinates3D(3, 2, 5).visibleSides(cubes), 6)
    assertEquals(Coordinates3D(2, 1, 5).visibleSides(cubes), 6)
    assertEquals(Coordinates3D(2, 3, 5).visibleSides(cubes), 6)
  }
  test("part 1") {
    assertEquals(Day18(Coordinates3D.fromLines(input)).part1, 64)
  }
  test("part 2") {
    assertEquals(Day18(Coordinates3D.fromLines(input)).part2, 58)
  }
  test("run") {
    assertEquals(Day18(Coordinates3D.fromLines(input)).run, (64, 58))
  }
}
