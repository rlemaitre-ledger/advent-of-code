package adventofcode.aoc2023.day13

import adventofcode.AoCTest

class Day13Test extends AoCTest:
  override val lines: String =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#""".stripMargin

  test("Part 1: 2nd pattern"):
    val pattern: List[String] =
      """#...##..#
        |#....#..#
        |..##..###
        |#####.##.
        |#####.##.
        |..##..###
        |#....#..#""".stripMargin.split("\n").toList
    assertEquals(Day13(pattern).part1, 400)
  test("Part 1: first pattern"):
    val pattern: List[String] =
      """#.##..##.
        |..#.##.#.
        |##......#
        |##......#
        |..#.##.#.
        |..##..##.
        |#.#.##.#.""".stripMargin.split("\n").toList
    assertEquals(Day13(pattern).part1, 5)
  test("Part 1: Complete"):
    assertEquals(Day13(input).part1, 405)

  test("Part 2: first pattern"):
    val pattern: List[String] =
      """#.##..##.
        |..#.##.#.
        |##......#
        |##......#
        |..#.##.#.
        |..##..##.
        |#.#.##.#.""".stripMargin.split("\n").toList
    assertEquals(Day13(pattern).part2, 300)

  test("Part 2: 2nd pattern"):
    val pattern: List[String] =
      """#...##..#
        |#....#..#
        |..##..###
        |#####.##.
        |#####.##.
        |..##..###
        |#....#..#""".stripMargin.split("\n").toList
    assertEquals(Day13(pattern).part2, 100)

  test("Part 2: Complete"):
    assertEquals(Day13(input).part2, 400)

  test("distance"):
    assertEquals(Pattern.substitutionDistance("###", "###"), 0)
    assertEquals(Pattern.substitutionDistance("###", ".##"), 1)
    assertEquals(Pattern.substitutionDistance("###", "#.#"), 1)
    assertEquals(Pattern.substitutionDistance("###", "##."), 1)
    assertEquals(Pattern.substitutionDistance("###", "..#"), 2)
    assertEquals(Pattern.substitutionDistance("###", ".#."), 2)
    assertEquals(Pattern.substitutionDistance("###", "#.."), 2)
    assertEquals(Pattern.substitutionDistance("###", "..."), 3)
