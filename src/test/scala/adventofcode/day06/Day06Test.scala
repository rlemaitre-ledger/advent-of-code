package adventofcode.day06

import adventofcode.AoCTest

class Day06Test extends AoCTest {

  override val lines: String = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

  test("valid buffer at start") {
    val buffer: Buffer = Buffer("abcdefgh", 0, Buffer.packetLength)
    assertEquals(buffer.isValid, true)
  }
  test("invalid buffer at start") {
    val buffer: Buffer = Buffer("aabcdefgh", 0, Buffer.packetLength)
    assertEquals(buffer.isValid, false)
  }
  test("part 1") {
    assertEquals(Day06.part1(List("mjqjpqmgbljsphdztnvjfqwrcgsmlb")), 7)
    assertEquals(Day06.part1(List("bvwbjplbgvbhsrlpgdmjqwftvncz")), 5)
    assertEquals(Day06.part1(List("nppdvjthqldpwncqszvftbrmjlhg")), 6)
    assertEquals(Day06.part1(List("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")), 10)
    assertEquals(Day06.part1(List("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")), 11)
  }
  test("part 2") {
    assertEquals(Day06.part2(List("mjqjpqmgbljsphdztnvjfqwrcgsmlb")), 19)
    assertEquals(Day06.part2(List("bvwbjplbgvbhsrlpgdmjqwftvncz")), 23)
    assertEquals(Day06.part2(List("nppdvjthqldpwncqszvftbrmjlhg")), 23)
    assertEquals(Day06.part2(List("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")), 29)
    assertEquals(Day06.part2(List("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")), 26)
  }
}
