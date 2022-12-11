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
  val monkeys: List[Monkey] = List(
    Monkey(
      number = 0,
      items = List(Item(79), Item(98)),
      operation = Multiply(19),
      test = Test(23, 2, 3),
      inspections = 0
    ),
    Monkey(
      number = 1,
      items = List(Item(54), Item(65), Item(75), Item(74)),
      operation = Increase(6),
      test = Test(19, 2, 0),
      inspections = 0
    ),
    Monkey(
      number = 2,
      items = List(Item(79), Item(60), Item(97)),
      operation = Square,
      test = Test(13, 1, 3),
      inspections = 0
    ),
    Monkey(number = 3, items = List(Item(74)), operation = Increase(3), test = Test(17, 0, 1), inspections = 0)
  )

  test("part 1") {
    assertEquals(Day11.part1(input), 10605)
  }
  test("part 2") {
    intercept[Throwable] {
      Day11.part2(input)
    }
  }
  test("parse monkeys") {
    assertEquals(Day11.monkeys(input), monkeys)
  }
  test("test item") {
    assertEquals(Test(2, 12, 42).test(Item(10)), 12)
    assertEquals(Test(2, 12, 42).test(Item(11)), 42)
  }
  test("increase operation") {
    assertEquals(Increase(1).modify(Item(10)), Item(11))
  }
  test("multiply operation") {
    assertEquals(Multiply(3).modify(Item(10)), Item(30))
  }
  test("square operation") {
    assertEquals(Square.modify(Item(10)), Item(100))
  }
  test("item after inspection") {
    assertEquals(Item(1500).afterInspection, Item(500))
    assertEquals(Item(1501).afterInspection, Item(500))
    assertEquals(Item(1502).afterInspection, Item(500))
    assertEquals(Item(1503).afterInspection, Item(501))
  }
  test("play round 1") {
    assertEquals(
      Day11.round(monkeys),
      List(
        Monkey(
          number = 0,
          items = List(Item(20), Item(23), Item(27), Item(26)),
          operation = Multiply(19),
          test = Test(23, 2, 3),
          inspections = 2
        ),
        Monkey(
          number = 1,
          items = List(Item(2080), Item(25), Item(167), Item(207), Item(401), Item(1046)),
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
}
