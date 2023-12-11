package adventofcode.aoc2023.day10

import adventofcode.AoCTest

class Day10Test extends AoCTest:
  override val lines: String =
    """
      |
      |""".stripMargin

  test("part 1"):
      val simple = """.....
                   |.S-7.
                   |.|.|.
                   |.L-J.
                   |.....""".stripMargin.split('\n').toList
      assertEquals(Day10(simple).part1, 4)
  test("part 1"):
      val simple = """-L|F7
                   |7S-7|
                   |L|7||
                   |-L-J|
                   |L|-JF""".stripMargin.split('\n').toList
      assertEquals(Day10(simple).part1, 4)
  test("part 1"):
      val simple = """..F7.
                   |.FJ|.
                   |SJ.L7
                   ||F--J
                   |LJ...""".stripMargin.split('\n').toList
      assertEquals(Day10(simple).part1, 8)

  test("part 2"):
      val simple = """...........
                   |.S-------7.
                   |.|F-----7|.
                   |.||.....||.
                   |.||.....||.
                   |.|L-7.F-J|.
                   |.|..|.|..|.
                   |.L--J.L--J.
                   |...........""".stripMargin.split('\n').toList
      assertEquals(Day10(simple).part2, 4)
  test("part 2"):
      val simple = """FF7FSF7F7F7F7F7F---7
                   |L|LJ||||||||||||F--J
                   |FL-7LJLJ||||||LJL-77
                   |F--JF--7||LJLJ7F7FJ-
                   |L---JF-JLJ.||-FJLJJ7
                   ||F|F-JF---7F7-L7L|7|
                   ||FFJF7L7F-JF7|JL---7
                   |7-L-JL7||F7|L7F-7F7|
                   |L.L7LFJ|||||FJL7||LJ
                   |L7JLJL-JLJLJL--JLJ.L""".stripMargin.split('\n').toList
      assertEquals(Day10(simple).part2, 10)
