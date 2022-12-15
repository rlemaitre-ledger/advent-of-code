package adventofcode.day03

import adventofcode.AoCTest
import adventofcode.Mode
import adventofcode.day03.Day03
import adventofcode.day03.Day03.*

class Day03Test extends AoCTest {

  val lines: String =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  test("Item has correct priority") {
    assertEquals(Item.priority('a'), 1)
    assertEquals(Item.priority('z'), 26)
    assertEquals(Item.priority('A'), 27)
    assertEquals(Item.priority('Z'), 52)
  }

  test("RugSack is well constructed") {
    assertEquals(
      RugSack.fromString("vJrwpWtwJgWrhcsFMMfFFhFp"),
      RugSack(
        Compartment.fromString("vJrwpWtwJgWr"),
        Compartment.fromString("hcsFMMfFFhFp")
      )
    )
  }

  test("Part 1") {
    val testInstance: Day03 = Day03(input.map(RugSack.fromString))
    assertEquals(testInstance.part1, 157)
  }

  test("groups are well computed") {
    val testInstance: Day03 = Day03(input.map(RugSack.fromString))
    val groups              = testInstance.groups
    assertEquals(groups.size, 2)
    assertEquals(groups.head.elves.size, 3)
    assertEquals(groups.last.elves.size, 3)
    assertEquals(groups.head.badge, Item.fromChar('r'))
    assertEquals(groups.last.badge, Item.fromChar('Z'))
  }
  test("Part 2") {
    val testInstance: Day03 = Day03(input.map(RugSack.fromString))
    assertEquals(testInstance.part2, 70)
  }
  test("answers") {
    assertEquals(Day03.instance.run(Mode.Part1), 7889)
    assertEquals(Day03.instance.run(Mode.Part2), 2825)
  }

}
