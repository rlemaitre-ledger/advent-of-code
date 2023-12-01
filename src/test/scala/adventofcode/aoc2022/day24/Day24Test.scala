package adventofcode.aoc2022.day24

import adventofcode.AoCTest

class Day24Test extends AoCTest {
  override val lines: String =
    """#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#""".stripMargin

  test("part 1") {
    assertEquals(Day24(Trip.parse(input)).part1, 18)
  }
  test("part 2") {
    intercept[NotImplementedError] {
      Day24(Trip.parse(input)).part2
    }
  }
}
