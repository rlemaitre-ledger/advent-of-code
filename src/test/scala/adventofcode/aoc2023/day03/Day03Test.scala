package adventofcode.aoc2023.day03

import adventofcode.AoCTest
import adventofcode.inputLines

class Day03Test extends AoCTest:
  override val lines: String =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..
      |""".stripMargin

  test("part 1") {
      assertEquals(Day03(Board.parse(input)).part1, 4361)
  }
  test("part 2") {
      assertEquals(Day03(Board.parse(input)).part2, 467835L)
  }
