package adventofcode.aoc2022.day01

import adventofcode.AoCTest
import adventofcode.Mode
import adventofcode.aoc2022.day01.Day01
import adventofcode.aoc2022.day01.Day01.*

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
  val day01: Day01 = Day01(elves(input))

  test("parse input") {
    assertEquals(elves(input), List(Elf(0, 6000), Elf(1, 4000), Elf(2, 11000), Elf(3, 24000), Elf(4, 10000)))
    assertEquals(elves(List("")), List(Elf(0, 0)))
  }
  test("part1") {
    assertEquals(day01.part1, 24000)
  }
  test("part2") {
    assertEquals(day01.part2, 45000)
  }
  test("real part 1") {
    assertEquals(Day01.instance.run(Mode.Part1), 66719)
  }
  test("real part 1") {
    assertEquals(Day01.instance.run(Mode.Part2), 198551)
  }
}
