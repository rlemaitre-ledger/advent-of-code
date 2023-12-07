package adventofcode.aoc2023.day07

import adventofcode.AoCTest

class Day07Test extends AoCTest:
  override val lines: String =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483
      |""".stripMargin

  test("part 1"):
      assertEquals(Day07(input).part1, 6440)

  test("part 2"):
      assertEquals(Day07(input).part2, 5905)
