package adventofcode

import adventofcode.AoCTest
import adventofcode.Day01.*

class Day01Test extends AoCTest {
  val lines: String = """1000
                |2000
                |3000
                |
                |4000
                |
                |5000
                |6000
                |
                |7000
                |8000
                |9000
                |
                |10000""".stripMargin

  test("parse input") {
    assertEquals(elves(input), List(Elf(0, 6000), Elf(1, 4000), Elf(2, 11000), Elf(3, 24000), Elf(4, 10000)))
    assertEquals(elves(List("")), List(Elf(0, 0)))
  }
  test("part1") {
    assertEquals(part1(input), 24000)
  }
  test("part2") {
    assertEquals(part2(input), 45000)
  }
}
