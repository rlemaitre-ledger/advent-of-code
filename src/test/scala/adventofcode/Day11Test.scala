package adventofcode

import adventofcode.Day11.*
import scala.collection.immutable.List
class Day11Test extends AoCTest {
  override val lines: String = """Monkey 0:
                                 |  Starting items: 79, 98
                                 |  Operation: new = old * 19
                                 |  Test: divisible by 23
                                 |    If true: throw to monkey 2
                                 |    If false: throw to monkey 3
                                 |
                                 |Monkey 1:
                                 |  Starting items: 54, 65, 75, 74
                                 |  Operation: new = old + 6
                                 |  Test: divisible by 19
                                 |    If true: throw to monkey 2
                                 |    If false: throw to monkey 0
                                 |
                                 |Monkey 2:
                                 |  Starting items: 79, 60, 97
                                 |  Operation: new = old * old
                                 |  Test: divisible by 13
                                 |    If true: throw to monkey 1
                                 |    If false: throw to monkey 3
                                 |
                                 |Monkey 3:
                                 |  Starting items: 74
                                 |  Operation: new = old + 3
                                 |  Test: divisible by 17
                                 |    If true: throw to monkey 0
                                 |    If false: throw to monkey 1""".stripMargin
  val round0: List[Monkey] = List(
    Monkey(
      number = 0,
      items = List(SimpleItem(79), SimpleItem(98)),
      operation = Multiply(19),
      test = Test(23, 2, 3),
      inspections = 0
    ),
    Monkey(
      number = 1,
      items = List(SimpleItem(54), SimpleItem(65), SimpleItem(75), SimpleItem(74)),
      operation = Increase(6),
      test = Test(19, 2, 0),
      inspections = 0
    ),
    Monkey(
      number = 2,
      items = List(SimpleItem(79), SimpleItem(60), SimpleItem(97)),
      operation = Square,
      test = Test(13, 1, 3),
      inspections = 0
    ),
    Monkey(number = 3, items = List(SimpleItem(74)), operation = Increase(3), test = Test(17, 0, 1), inspections = 0)
  )
  val round1: List[Monkey] = List(
    Monkey(
      number = 0,
      items = List(60, 71, 81, 80).map(SimpleItem.apply),
      operation = Multiply(19),
      test = Test(23, 2, 3),
      inspections = 2
    ),
    Monkey(
      number = 1,
      items = List(77, 1504, 1865, 6244, 3603, 9412).map(SimpleItem.apply),
      operation = Increase(6),
      test = Test(19, 2, 0),
      inspections = 4
    ),
    Monkey(
      number = 2,
      items = List.empty,
      operation = Square,
      test = Test(13, 1, 3),
      inspections = 3
    ),
    Monkey(number = 3, items = List.empty, operation = Increase(3), test = Test(17, 0, 1), inspections = 6)
  )
  val round2: List[Monkey] = List(
    Monkey(
      number = 0,
      items = List(83, 1510, 1871, 6250, 3609, 9418).map(SimpleItem.apply),
      operation = Multiply(19),
      test = Test(23, 2, 3),
      inspections = 6
    ),
    Monkey(
      number = 1,
      items = List(1143, 1352, 1542, 1523).map(SimpleItem.apply),
      operation = Increase(6),
      test = Test(19, 2, 0),
      inspections = 10
    ),
    Monkey(
      number = 2,
      items = List.empty,
      operation = Square,
      test = Test(13, 1, 3),
      inspections = 3
    ),
    Monkey(number = 3, items = List.empty, operation = Increase(3), test = Test(17, 0, 1), inspections = 10)
  )
  val round3: List[Monkey] = List(
    Monkey(
      number = 0,
      items = List(1149, 1358, 1548, 1529).map(SimpleItem.apply),
      operation = Multiply(19),
      test = Test(23, 2, 3),
      inspections = 12
    ),
    Monkey(
      number = 1,
      items = List(1580, 28693, 35552, 118753, 68574, 178945).map(SimpleItem.apply),
      operation = Increase(6),
      test = Test(19, 2, 0),
      inspections = 14
    ),
    Monkey(
      number = 2,
      items = List.empty,
      operation = Square,
      test = Test(13, 1, 3),
      inspections = 3
    ),
    Monkey(number = 3, items = List.empty, operation = Increase(3), test = Test(17, 0, 1), inspections = 16)
  )
  val round4: List[Monkey] = List(
    Monkey(
      number = 0,
      items = List(1586, 28699, 35558, 118759, 68580, 178951).map(SimpleItem.apply),
      operation = Multiply(19),
      test = Test(23, 2, 3),
      inspections = 16
    ),
    Monkey(
      number = 1,
      items = List(21834, 25805, 29415, 29054).map(SimpleItem.apply),
      operation = Increase(6),
      test = Test(19, 2, 0),
      inspections = 20
    ),
    Monkey(
      number = 2,
      items = List.empty,
      operation = Square,
      test = Test(13, 1, 3),
      inspections = 3
    ),
    Monkey(number = 3, items = List.empty, operation = Increase(3), test = Test(17, 0, 1), inspections = 20)
  )

  test("part 1") {
    assertEquals(Day11.part1(input), 10605L)
  }
  test("part 2") {
    intercept[Throwable] {
      Day11.part2(input)
    }
  }
  test("parse monkeys") {
    assertEquals(Day11.monkeys(input, Mode.Part1), round0)
  }
  test("test item") {
    assertEquals(Test(2, 12, 42).test(SimpleItem(10)), 12)
    assertEquals(Test(2, 12, 42).test(SimpleItem(11)), 42)
  }
  test("increase operation") {
    assertEquals(Increase(1).modify(SimpleItem(10)), SimpleItem(11))
  }
  test("multiply operation") {
    assertEquals(Multiply(3).modify(SimpleItem(10)), SimpleItem(30))
  }
  test("square operation") {
    assertEquals(Square.modify(SimpleItem(10)), SimpleItem(100))
  }
  test("play round 1") {
    assertEquals(
      Day11.round(round0),
      List(
        Monkey(
          number = 0,
          items = List(SimpleItem(20), SimpleItem(23), SimpleItem(27), SimpleItem(26)),
          operation = Multiply(19),
          test = Test(23, 2, 3),
          inspections = 2
        ),
        Monkey(
          number = 1,
          items =
            List(SimpleItem(2080), SimpleItem(25), SimpleItem(167), SimpleItem(207), SimpleItem(401), SimpleItem(1046)),
          operation = Increase(6),
          test = Test(19, 2, 0),
          inspections = 4
        ),
        Monkey(
          number = 2,
          items = List.empty,
          operation = Square,
          test = Test(13, 1, 3),
          inspections = 3
        ),
        Monkey(number = 3, items = List.empty, operation = Increase(3), test = Test(17, 0, 1), inspections = 5)
      )
    )
  }
  test("inspections with new rules") {
    testInspections(1, List(2L, 4L, 3L, 6L))
    testInspections(20, List(99L, 97L, 8L, 103L))
    testInspections(1000, List(5204L, 4792L, 199L, 5192L))
    testInspections(2000, List(10419L, 9577L, 392L, 10391L))
    testInspections(10000, List(52166L, 47830L, 1938L, 52013L))
  }
  def testInspections(nbRounds: Int, expected: List[Long]): Unit =
    assertEquals(
      play(Day11.monkeys(input, Mode.Part2), nbRounds).map(_.inspections),
      expected
    )
}
