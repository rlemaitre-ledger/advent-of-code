package adventofcode.aoc2023.day06

import adventofcode.AoCTest

class Day06Test extends AoCTest:
  override val lines: String =
    """Time:      7  15   30
      |Distance:  9  40  200
      |""".stripMargin

  test("part 1") {
    assertEquals(Day06(input).part1, 288L)
  }

  test("part 2") {
    assertEquals(Day06(input).part2, 71503L)
  }
