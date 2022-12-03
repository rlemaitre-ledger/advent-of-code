package adventofcode.day1

import Day1.*
import munit.FunSuite

class Day1Test extends FunSuite {
  val input = """1000
                |2000
                |3000
                |
                |4000
                |
                |5000
                |6000
                |
                |7000
                |8000
                |9000
                |
                |10000""".stripMargin.split('\n').toList
  test("parse input") {
    assertEquals(caloriesByElf(input), List((6000L, 0), (4000L, 1), (11000L, 2), (24000L, 3), (10000L, 4)))
  }
  test("part1") {
    assertEquals(maxCalories(caloriesByElf(input)), 24000L)
  }
  test("part2") {
    assertEquals(top3ElvesCalories(caloriesByElf(input)), 45000L)
  }
}
