package adventofcode.aoc2023.day16

import adventofcode.AoCTest

class Day16Test extends AoCTest:
  override val lines: String =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin

  test("part 1"):
      assertEquals(Day16(input).part1, 46)

  test("part 2"):
      assertEquals(Day16(input).part2, 51)
