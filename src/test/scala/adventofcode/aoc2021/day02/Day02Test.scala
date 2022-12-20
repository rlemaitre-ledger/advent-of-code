package adventofcode.aoc2021.day02

import adventofcode.AoCTest
import adventofcode.aoc2021.day02.Day02.*

class Day02Test extends AoCTest {
  override val lines: String =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin

  test("part 1") {
    assertEquals(Day02(parse(input)).part1, 150)
  }
  test("part 2") {
    assertEquals(Day02(parse(input)).part2, 900)
  }
}
