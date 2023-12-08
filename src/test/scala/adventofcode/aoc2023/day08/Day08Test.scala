package adventofcode.aoc2023.day08

import adventofcode.AoCTest

class Day08Test extends AoCTest:
  override val lines: String =
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)
      |""".stripMargin

  test("part 1"):
      assertEquals(Day08(input).part1, BigInt(2))

  test("part 2"):
      val input2 = """LR
                   |
                   |11A = (11B, XXX)
                   |11B = (XXX, 11Z)
                   |11Z = (11B, XXX)
                   |22A = (22B, XXX)
                   |22B = (22C, 22C)
                   |22C = (22Z, 22Z)
                   |22Z = (22B, 22B)
                   |XXX = (XXX, XXX)
                   |""".stripMargin
      assertEquals(Day08(input2.split('\n').toList).part2, BigInt(6))
