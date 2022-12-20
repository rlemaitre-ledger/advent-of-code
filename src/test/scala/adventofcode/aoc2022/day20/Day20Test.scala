package adventofcode.aoc2022.day20

import adventofcode.AoCTest
import adventofcode.aoc2022.day20.Day20.*

class Day20Test extends AoCTest {
  override val lines: String = """1
                                 |2
                                 |-3
                                 |3
                                 |-2
                                 |0
                                 |4""".stripMargin
  test("part 1") {
    assertEquals(Day20(input).part1, 3L)
  }
  test("encrypted rotated file value at") {
    val prob = EncryptedFile.parse(input).rotate()
    assertEquals(prob.valueAt(1000), 4L)
    assertEquals(prob.valueAt(2000), -3L)
    assertEquals(prob.valueAt(3000), 2L)
  }
  test("part 2") {
    assertEquals(Day20(input).part2, 1623178306L)
  }
  test("encrypted 10-times-rotated file value at") {
    val prob = EncryptedFile.parse(input, decryptionKey).rotate(10)
    assertEquals(prob.valueAt(1000), 811589153L)
    assertEquals(prob.valueAt(2000), 2434767459L)
    assertEquals(prob.valueAt(3000), -1623178306L)
  }
}
