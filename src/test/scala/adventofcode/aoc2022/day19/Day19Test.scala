package adventofcode.aoc2022.day19

import adventofcode.AoCTest
import adventofcode.aoc2022.day19.Day19.*
class Day19Test extends AoCTest {
  override val lines: String =
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin
  val blueprint1: Blueprint = Blueprint(
    1,
    Resources(4, 0, 0, 0),
    Resources(2, 0, 0, 0),
    Resources(3, 14, 0, 0),
    Resources(2, 0, 7, 0)
  )
  val blueprint2: Blueprint = Blueprint(
    2,
    Resources(2, 0, 0, 0),
    Resources(3, 0, 0, 0),
    Resources(3, 8, 0, 0),
    Resources(3, 0, 12, 0)
  )
  val blueprints: List[Blueprint] = List(blueprint1, blueprint2)

  test("parse") {
    assertEquals(Blueprint.parse(input), blueprints)
  }
  test("part 1") {
    assertEquals(Day19(blueprints).part1, 33)
  }
  test("part 2") {
    assertEquals(Day19(blueprints).part2, 56L * 62L)
  }
}
