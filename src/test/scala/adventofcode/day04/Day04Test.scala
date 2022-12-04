package adventofcode.day04

import adventofcode.AoCTest

class Day04Test extends AoCTest {
  val lines: String = """2-4,6-8
                        |2-3,4-5
                        |5-7,7-9
                        |2-8,3-7
                        |6-6,4-6
                        |2-6,4-8
                        |""".stripMargin
  test("Construct assignment from input") {
    assertEquals(Assignment.from("2-4"), Assignment(2, 4))
  }

  test("Construct pair from input") {
    assertEquals(Pair.from("2-4,6-8"), Pair(Assignment(2, 4), Assignment(6, 8)))
  }

  test("Construct pairs from input") {
    assertEquals(
      Day04.pairs(input),
      List(
        Pair(Assignment(2, 4), Assignment(6, 8)),
        Pair(Assignment(2, 3), Assignment(4, 5)),
        Pair(Assignment(5, 7), Assignment(7, 9)),
        Pair(Assignment(2, 8), Assignment(3, 7)),
        Pair(Assignment(6, 6), Assignment(4, 6)),
        Pair(Assignment(2, 6), Assignment(4, 8))
      )
    )
  }

  test("Two distinct assignments are not included") {
    assertEquals(Pair(Assignment(0, 3), Assignment(5, 9)).assignmentsIncluded, false)
  }
  test("Two equals assignments are included") {
    assertEquals(Pair(Assignment(0, 3), Assignment(0, 3)).assignmentsIncluded, true)
  }
  test("First includes second") {
    assertEquals(Pair(Assignment(0, 9), Assignment(1, 5)).assignmentsIncluded, true)
  }
  test("Second includes first") {
    assertEquals(Pair(Assignment(1, 5), Assignment(0, 9)).assignmentsIncluded, true)
  }
  test("part 1") {
    assertEquals(Day04.part1(input), 2)
  }
}
