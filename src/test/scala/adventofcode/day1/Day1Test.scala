package adventofcode.day1

import Day1.*
import munit.FunSuite

class Day1Test extends FunSuite {
  val input = """1000
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
                |10000""".stripMargin.split('\n').toList

  test("parse input") {
    assertEquals(elves(input), List(Elf(0, 6000L), Elf(1, 4000L), Elf(2, 11000L), Elf(3, 24000L), Elf(4, 10000L)))
    assertEquals(elves(List("")), List(Elf(0, 0L)))
  }
  test("part1") {
    assertEquals(part1(input), 24000L)
  }
  test("part2") {
    assertEquals(part2(input), 45000L)
  }
}
