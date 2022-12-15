package adventofcode.day15

import adventofcode.*
import adventofcode.day15.Day15
import adventofcode.day15.Day15.*

class Day15Test extends AoCTest {
  override val lines: String = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                                 |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                                 |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                                 |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                                 |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                                 |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                                 |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                                 |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                                 |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                                 |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                                 |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                                 |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                                 |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                                 |Sensor at x=20, y=1: closest beacon is at x=15, y=3
                                 |""".stripMargin
  val testInstance: Day15 = Day15(sensors(input), 10, 20)
  test("part 1") {
    assertEquals(testInstance.positionWithoutBeacon(None), 26)
  }
  test("within radius at line") {
    assertEquals(
      Coordinates.origin.withinInLine(2, 1),
      Set(
        Coordinates(-1, 1),
        Coordinates(1, 1),
        Coordinates(0, 1)
      )
    )
    assertEquals(
      Coordinates.origin.withinInLine(2, -1),
      Set(
        Coordinates(-1, -1),
        Coordinates(1, -1),
        Coordinates(0, -1)
      )
    )
  }
  test("remove from MultiRange") {
    val multiRange = MultiRange(List(IntRange(-2, 24)))
    assertEquals(multiRange.remove(2), MultiRange(List(IntRange(-2, 1), IntRange(3, 24))))
  }
  test("possible solutions") {
    val solutions = testInstance.possibleSolutions(sensors(input), 0, 20)
    assertEquals(solutions.size, 1)
    assertEquals(solutions.head._1, 11)
    assertEquals(solutions.head._2.free, Some(14))
  }
  test("range overlap") {
    assertEquals(IntRange(0, 10).union(IntRange(5, 15)), (IntRange(0, 15), None))
  }
  test("range remove") {
    val m1 = MultiRange(List(IntRange(0, 10)))
    assertEquals(m1.remove(0), MultiRange(List(IntRange(1, 10))))
    assertEquals(m1.remove(10), MultiRange(List(IntRange(0, 9))))
    assertEquals(m1.remove(-2), MultiRange(List(IntRange(0, 10))))
    assertEquals(m1.remove(12), MultiRange(List(IntRange(0, 10))))
    assertEquals(m1.remove(5), MultiRange(List(IntRange(0, 4), IntRange(6, 10))))
  }
  test("tuning frequency") {
    assertEquals(testInstance.tuningFrequency, 56000011L)
  }
  test("limit range") {
    val limits = Some(0, 20)
    assertEquals(IntRange(-10, 200).limit(limits), Some(IntRange(0, 20)))
    assertEquals(IntRange(-10, 0).limit(limits), Some(IntRange(0, 0)))
    assertEquals(IntRange(0, 200).limit(limits), Some(IntRange(0, 20)))
    assertEquals(IntRange(5, 200).limit(limits), Some(IntRange(5, 20)))
    assertEquals(IntRange(-300, -200).limit(limits), None)
    assertEquals(IntRange(100, 200).limit(limits), None)
  }
  test("add range outside limits") {
    val m = MultiRange.empty(Some(0, 20))
    assertEquals(m.add(IntRange(100, 200)), m)
  }
  test("part 1") {
    assertEquals(testInstance.part1, 26)
  }
  test("part 2") {
    assertEquals(testInstance.part2, 56000011L)
  }
}
