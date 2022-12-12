package adventofcode

import adventofcode.AoCTest
import adventofcode.Day03.*

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
    assertEquals(Day03.part1(input), 157)
  }

  test("groups are well computed") {
    val groups = Day03.groups(input)
    assertEquals(groups.size, 2)
    assertEquals(groups.head.elves.size, 3)
    assertEquals(groups.last.elves.size, 3)
    assertEquals(groups.head.badge, Item.fromChar('r'))
    assertEquals(groups.last.badge, Item.fromChar('Z'))
  }
  test("Part 2") {
    assertEquals(Day03.part2(input), 70)
  }
  test("answers") {
    assertEquals(Day03.run(Mode.Part1), 7889)
    assertEquals(Day03.run(Mode.Part2), 2825)
  }

}
