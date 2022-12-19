package adventofcode.aoc2022.day06

import adventofcode.AoCTest
import adventofcode.Mode
import adventofcode.aoc2022.day06.Day06
import adventofcode.aoc2022.day06.Day06.*

class Day06Test extends AoCTest {

  override val lines: String = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  val testInstance: Day06    = Day06(lines)

  test("valid buffer at start") {
    val buffer: Buffer = Buffer("abcdefgh", 0, Buffer.packetLength)
    assertEquals(buffer.isValid, true)
  }
  test("invalid buffer at start") {
    val buffer: Buffer = Buffer("aabcdefgh", 0, Buffer.packetLength)
    assertEquals(buffer.isValid, false)
  }
  test("part 1") {
    assertEquals(Day06("mjqjpqmgbljsphdztnvjfqwrcgsmlb").part1, 7)
    assertEquals(Day06("bvwbjplbgvbhsrlpgdmjqwftvncz").part1, 5)
    assertEquals(Day06("nppdvjthqldpwncqszvftbrmjlhg").part1, 6)
    assertEquals(Day06("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg").part1, 10)
    assertEquals(Day06("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw").part1, 11)
  }
  test("part 2") {
    assertEquals(Day06("mjqjpqmgbljsphdztnvjfqwrcgsmlb").part2, 19)
    assertEquals(Day06("bvwbjplbgvbhsrlpgdmjqwftvncz").part2, 23)
    assertEquals(Day06("nppdvjthqldpwncqszvftbrmjlhg").part2, 23)
    assertEquals(Day06("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg").part2, 29)
    assertEquals(Day06("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw").part2, 26)
  }
  test("answers") {
    assertEquals(Day06.instance.run(Mode.Part1), 1953)
    assertEquals(Day06.instance.run(Mode.Part2), 2301)
  }
}
