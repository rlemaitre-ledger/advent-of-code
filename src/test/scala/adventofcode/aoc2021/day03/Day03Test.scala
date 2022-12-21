package adventofcode.aoc2021.day03

import adventofcode.AoCTest

class Day03Test extends AoCTest {
  override val lines: String =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin

  test("part 1") {
    assertEquals(Day03(input).part1, 198)
  }
  test("part 2") {
    assertEquals(Day03(input).part2, 230)
  }
}
