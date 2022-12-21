package adventofcode.aoc2022.day21

import adventofcode.AoCTest

class Day21Test extends AoCTest {
  override val lines: String =
    """root: pppw + sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 5
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32""".stripMargin

  private val problem: Day21 = Day21(Monkey.parse(input))
  val root: OperationMonkey  = problem.input("root").asInstanceOf[OperationMonkey]
  test("part 1") {
    assertEquals(problem.part1, BigDecimal(152))
  }
  test("part 2") {
    assertEquals(problem.part2, BigDecimal(301))
  }
  test("find value") {
    assertEquals(problem.findValue(root, "humn"), BigDecimal(301))
    assertEquals(problem.findValue(root, "zczc"), BigDecimal(31.6))
    assertEquals(problem.findValue(root, "lfqf").toInt, 0)
    assertEquals(problem.findValue(root, "sllz"), BigDecimal(596))
  }
}
