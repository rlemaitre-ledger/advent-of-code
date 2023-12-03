package adventofcode.aoc2023.day02

import adventofcode.AoCTest
import adventofcode.aoc2023.day02.Day02.Game

class Day02Test extends AoCTest:
  override val lines: String =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      |""".stripMargin

  test("part 1") {
    assertEquals(Day02(input.map(Game.parse)).part1, 8)
  }
  test("part 2") {
    assertEquals(Day02(input.map(Game.parse)).part2, 2286L)
  }
