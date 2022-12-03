package adventofcode.day3

import munit.FunSuite

class Day3Test extends FunSuite {
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
    val lines = """vJrwpWtwJgWrhcsFMMfFFhFp
                  |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                  |PmmdzqPrVvPwwTWBwg
                  |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                  |ttgJtRGJQctTZtZT
                  |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.split('\n').toList
    assertEquals(Part1.totalPriority(lines), 157)
  }

  test("groups are well computed") {
    val lines = """vJrwpWtwJgWrhcsFMMfFFhFp
                  |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                  |PmmdzqPrVvPwwTWBwg
                  |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                  |ttgJtRGJQctTZtZT
                  |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.split('\n').toList
    val groups = Part2.groups(lines)
    assertEquals(groups.size, 2)
    assertEquals(groups.head.elves.size, 3)
    assertEquals(groups.last.elves.size, 3)
    assertEquals(groups.head.badge, Item.fromChar('r'))
    assertEquals(groups.last.badge, Item.fromChar('Z'))
    assertEquals(Part2.priority(groups), 70)
  }
}
