package adventofcode.day03

import adventofcode.AoCTest

class Day03Test extends AoCTest {

  val lines =
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

  test("Total priority is well computed") {
    assertEquals(Part1.totalPriority(input), 157)
  }

  test("groups are well computed") {
    val groups = Part2.groups(input)
    assertEquals(groups.size, 2)
    assertEquals(groups.head.elves.size, 3)
    assertEquals(groups.last.elves.size, 3)
    assertEquals(groups.head.badge, Item.fromChar('r'))
    assertEquals(groups.last.badge, Item.fromChar('Z'))
    assertEquals(Part2.priority(groups), 70)
  }
}
