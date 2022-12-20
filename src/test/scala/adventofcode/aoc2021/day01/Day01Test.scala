package adventofcode.aoc2021.day01

import adventofcode.AoCTest

class Day01Test extends AoCTest {
  override val lines: String =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin

  test("part 1") {
      assertEquals(Day01(input).part1, 7)
  }
  test("part 2") {
    assertEquals(Day01(input).part2, 5)
  }
}
