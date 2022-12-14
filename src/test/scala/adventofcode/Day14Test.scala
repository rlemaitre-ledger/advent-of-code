package adventofcode

import Day14.*
class Day14Test extends AoCTest {
  override val lines: String = """498,4 -> 498,6 -> 496,6
                                 |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin
  test("part 1") {
    assertEquals(part1(input), 24)
  }
  test("part 2") {
    assertEquals(part2(input), 93)
  }
  test("parse invalid path") {
    intercept[IllegalArgumentException] {
      paths(List("toto"))
    }
  }
  test("parse paths") {
    assertEquals(
      paths(input),
      List(
        Path(List(Coordinates(498, 4), Coordinates(498, 6), Coordinates(496, 6))),
        Path(List(Coordinates(503, 4), Coordinates(502, 4), Coordinates(502, 9), Coordinates(494, 9)))
      )
    )
  }
  test("build walls") {
    val path = Path(List(Coordinates(498, 4), Coordinates(498, 6), Coordinates(496, 6)))
    assertEquals(
      path.wallCells,
      Set(
        Coordinates(498, 4),
        Coordinates(498, 5),
        Coordinates(498, 6),
        Coordinates(497, 6),
        Coordinates(496, 6)
      )
    )
    val path2 = Path(List(Coordinates(503, 4), Coordinates(502, 4), Coordinates(502, 9), Coordinates(494, 9)))
    assertEquals(
      path2.wallCells,
      Set(
        Coordinates(503, 4),
        Coordinates(502, 4),
        Coordinates(502, 5),
        Coordinates(502, 6),
        Coordinates(502, 7),
        Coordinates(502, 8),
        Coordinates(502, 9),
        Coordinates(501, 9),
        Coordinates(500, 9),
        Coordinates(499, 9),
        Coordinates(498, 9),
        Coordinates(497, 9),
        Coordinates(496, 9),
        Coordinates(495, 9),
        Coordinates(494, 9)
      )
    )
  }
}
