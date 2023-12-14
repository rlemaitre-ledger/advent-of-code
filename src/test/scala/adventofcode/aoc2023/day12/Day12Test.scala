package adventofcode.aoc2023.day12

import adventofcode.AoCTest

class Day12Test extends AoCTest:
  override val lines: String =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1
      |""".stripMargin

  test("part 1 example 1"):
    assertEquals(Line.combinations(Line.parse(input.head)), 1)

  test("part 1 example 2"):
    assertEquals(Line.combinations(Line.parse(input(1))), 4)

  test("part 1 example 3"):
    assertEquals(Line.combinations(Line.parse(input(2))), 1)

  test("part 1 example 4"):
    assertEquals(Line.combinations(Line.parse(input(3))), 1)

  test("part 1 example 5"):
    assertEquals(Line.combinations(Line.parse(input(4))), 4)

  test("part 1 example 6"):
    assertEquals(Line.combinations(Line.parse(input(5))), 10)

  test("part 1"):
    assertEquals(Day12(input).part1, 21)

  test("part 2 example 1"):
    assertEquals(Line.combinations(Line.parse(input.head) * 5), 1)

  test("part 2 example 2"):
    assertEquals(Line.combinations(Line.parse(input(1)) * 5), 16384)

  test("part 2 example 3"):
    assertEquals(Line.combinations(Line.parse(input(2)) * 5), 1)

  test("part 2 example 4"):
    assertEquals(Line.combinations(Line.parse(input(3)) * 5), 16)

  test("part 2 example 5"):
    assertEquals(Line.combinations(Line.parse(input(4)) * 5), 2500)

  test("part 2 example 6"):
    assertEquals(Line.combinations(Line.parse(input(5)) * 5), 506250)

  test("part 2"):
    assertEquals(Day12(input).part2, 525152)

  test("multiply test"):
    val line = Line.parse(".#?.#?.#?.#?.# 1,1,1,1,1") * 5
    val candidateLength = line.str.length
    val damagedLength = line.damaged.length
    assertEquals(candidateLength, damagedLength)