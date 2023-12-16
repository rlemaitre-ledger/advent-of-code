package adventofcode.aoc2023.day14

import adventofcode.AoCTest

class Day14Test extends AoCTest:
  override val lines: String =
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....""".stripMargin

  test("part 1"):
      assertEquals(Day14(input).part1, 136)

  test("part 2"):
      assertEquals(Day14(input).part2, 64)

  test("rotate"):
      val expected = """.....#....
                     |....#...O#
                     |...OO##...
                     |.OO#......
                     |.....OOO#.
                     |.O#...O#.#
                     |....O#....
                     |......OOOO
                     |#...O###..
                     |#..OO#....""".stripMargin
      assertEquals(Platform.parse(input).cycle.show, expected)

  test("rotate twice"):
      val expected = """.....#....
                     |....#...O#
                     |.....##...
                     |..O#......
                     |.....OOO#.
                     |.O#...O#.#
                     |....O#...O
                     |.......OOO
                     |#..OO###..
                     |#.OOO#...O""".stripMargin
      assertEquals(Platform.parse(input).cycle.cycle.show, expected)

  test("rotate three time"):
      val expected = """.....#....
                     |....#...O#
                     |.....##...
                     |..O#......
                     |.....OOO#.
                     |.O#...O#.#
                     |....O#...O
                     |.......OOO
                     |#...O###.O
                     |#.OOO#...O""".stripMargin
      assertEquals(Platform.parse(input).cycle.cycle.cycle.show, expected)
