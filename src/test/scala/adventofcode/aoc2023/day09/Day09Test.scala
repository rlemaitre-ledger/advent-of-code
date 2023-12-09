package adventofcode.aoc2023.day09

import adventofcode.AoCTest
import adventofcode.aoc2023.day09.Day09.Line

class Day09Test extends AoCTest:
  override val lines: String =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45
      |""".stripMargin

  test("difference"):
      assertEquals(Line(List(1, 2, 3)).difference, Line(List(1, 1)))
      assertEquals(Line(List(-1, -2, -3)).difference, Line(List(-1, -1)))
      assertEquals(Line(List(-3, 2, -42)).difference, Line(List(5, -44)))
  test("plus"):
      assertEquals(Line(List(0, 3, 6, 9, 12, 15)) + Line(List(3, 3, 3, 3, 3)), Line(List(-3, 0, 3, 6, 9, 12, 15, 18)))
  test("nextValue"):
      assertEquals(Line(List(1, 2, 3)).nextValue, 4)
      assertEquals(Line(List(0, 3, 6, 9, 12, 15)).nextValue, 18)
  test("part 1"):
      assertEquals(Day09(input).part1, 114)

  test("part 2"):
      assertEquals(Day09(input).part2, 2)
