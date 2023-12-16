package adventofcode.aoc2023.day15

import adventofcode.AoCTest

class Day15Test extends AoCTest:
  override val lines: String =
    """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7""".stripMargin

  test("hash"):
    assertEquals(Day15.hash("rn=1"), 30)
    assertEquals(Day15.hash("cm-"), 253)
    assertEquals(Day15.hash("qp=3"), 97)
    assertEquals(Day15.hash("cm=2"), 47)
    assertEquals(Day15.hash("qp-"), 14)
    assertEquals(Day15.hash("pc=4"), 180)
    assertEquals(Day15.hash("ot=9"), 9)
    assertEquals(Day15.hash("ab=5"), 197)
    assertEquals(Day15.hash("pc-"), 48)
    assertEquals(Day15.hash("pc=6"), 214)
    assertEquals(Day15.hash("ot=7"), 231)

  test("part 1"):
    assertEquals(Day15(input).part1, 1320)

  test("part 2"):
    assertEquals(Day15(input).part2, 145)
