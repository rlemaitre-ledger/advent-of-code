package adventofcode.aoc2022.day17

import adventofcode.*
import adventofcode.aoc2022.day17.Day17
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class Day17Test extends AoCTest {
  override val lines: String = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  test("part 1") {
    assertEquals(Day17(lines).part1, 3068)
  }
  test("height after rocks") {
    assertEquals(Day17(lines).heightAfter(1), 1)
    assertEquals(Day17(lines).heightAfter(2), 4)
    assertEquals(Day17(lines).heightAfter(3), 6)
    assertEquals(Day17(lines).heightAfter(4), 7)
    assertEquals(Day17(lines).heightAfter(5), 9)
    assertEquals(Day17(lines).heightAfter(6), 10)
    assertEquals(Day17(lines).heightAfter(7), 13)
    assertEquals(Day17(lines).heightAfter(8), 15)
    assertEquals(Day17(lines).heightAfter(9), 17)
    assertEquals(Day17(lines).heightAfter(10), 17)
  }
  test("part 2") {
    assertEquals(Day17(lines).part2, 1514285714288L)
  }

}
