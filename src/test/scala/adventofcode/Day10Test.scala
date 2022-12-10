package adventofcode

import adventofcode.Day10.*

class Day10Test extends AoCTest {
  override val lines: String = """addx 15
                                 |addx -11
                                 |addx 6
                                 |addx -3
                                 |addx 5
                                 |addx -1
                                 |addx -8
                                 |addx 13
                                 |addx 4
                                 |noop
                                 |addx -1
                                 |addx 5
                                 |addx -1
                                 |addx 5
                                 |addx -1
                                 |addx 5
                                 |addx -1
                                 |addx 5
                                 |addx -1
                                 |addx -35
                                 |addx 1
                                 |addx 24
                                 |addx -19
                                 |addx 1
                                 |addx 16
                                 |addx -11
                                 |noop
                                 |noop
                                 |addx 21
                                 |addx -15
                                 |noop
                                 |noop
                                 |addx -3
                                 |addx 9
                                 |addx 1
                                 |addx -3
                                 |addx 8
                                 |addx 1
                                 |addx 5
                                 |noop
                                 |noop
                                 |noop
                                 |noop
                                 |noop
                                 |addx -36
                                 |noop
                                 |addx 1
                                 |addx 7
                                 |noop
                                 |noop
                                 |noop
                                 |addx 2
                                 |addx 6
                                 |noop
                                 |noop
                                 |noop
                                 |noop
                                 |noop
                                 |addx 1
                                 |noop
                                 |noop
                                 |addx 7
                                 |addx 1
                                 |noop
                                 |addx -13
                                 |addx 13
                                 |addx 7
                                 |noop
                                 |addx 1
                                 |addx -33
                                 |noop
                                 |noop
                                 |noop
                                 |addx 2
                                 |noop
                                 |noop
                                 |noop
                                 |addx 8
                                 |noop
                                 |addx -1
                                 |addx 2
                                 |addx 1
                                 |noop
                                 |addx 17
                                 |addx -9
                                 |addx 1
                                 |addx 1
                                 |addx -3
                                 |addx 11
                                 |noop
                                 |noop
                                 |addx 1
                                 |noop
                                 |addx 1
                                 |noop
                                 |noop
                                 |addx -13
                                 |addx -19
                                 |addx 1
                                 |addx 3
                                 |addx 26
                                 |addx -30
                                 |addx 12
                                 |addx -1
                                 |addx 3
                                 |addx 1
                                 |noop
                                 |noop
                                 |noop
                                 |addx -9
                                 |addx 18
                                 |addx 1
                                 |addx 2
                                 |noop
                                 |noop
                                 |addx 9
                                 |noop
                                 |noop
                                 |noop
                                 |addx -1
                                 |addx 2
                                 |addx -37
                                 |addx 1
                                 |addx 3
                                 |noop
                                 |addx 15
                                 |addx -21
                                 |addx 22
                                 |addx -6
                                 |addx 1
                                 |noop
                                 |addx 2
                                 |addx 1
                                 |noop
                                 |addx -10
                                 |noop
                                 |noop
                                 |addx 20
                                 |addx 1
                                 |addx 2
                                 |addx 2
                                 |addx -6
                                 |addx -11
                                 |noop
                                 |noop
                                 |noop""".stripMargin

  test("Simple Register") {
    val simple = """noop
                   |addx 3
                   |addx -5
                   |""".stripMargin.split('\n').toList
    val executions = Day10.executions(simple)
    assertEquals(Day10.registerAt(Cycle(1), executions), Register(1))
    assertEquals(Day10.registerAt(Cycle(2), executions), Register(1))
    assertEquals(Day10.registerAt(Cycle(3), executions), Register(1))
    assertEquals(Day10.registerAt(Cycle(4), executions), Register(4))
    assertEquals(Day10.registerAt(Cycle(5), executions), Register(4))
    assertEquals(Day10.registerAt(Cycle(6), executions), Register(-1))
  }
  test("Register") {
    val executions = Day10.executions(input)
    assertEquals(Day10.registerAt(Cycle(20), executions), Register(21))
    assertEquals(Day10.registerAt(Cycle(60), executions), Register(19))
    assertEquals(Day10.registerAt(Cycle(100), executions), Register(18))
    assertEquals(Day10.registerAt(Cycle(140), executions), Register(21))
    assertEquals(Day10.registerAt(Cycle(180), executions), Register(16))
    assertEquals(Day10.registerAt(Cycle(220), executions), Register(18))
  }
  test("Signal Strength") {
    val executions = Day10.executions(input)
    assertEquals(Day10.signal(Cycle(20), executions).strength, 420)
    assertEquals(Day10.signal(Cycle(60), executions).strength, 1140)
    assertEquals(Day10.signal(Cycle(100), executions).strength, 1800)
    assertEquals(Day10.signal(Cycle(140), executions).strength, 2940)
    assertEquals(Day10.signal(Cycle(180), executions).strength, 2880)
    assertEquals(Day10.signal(Cycle(220), executions).strength, 3960)
  }
  test("part 1") {
    assertEquals(Day10.part1(input), 13140)
  }
  test("display to string") {
    assertEquals(
      Display.default.toString,
      """........................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    assertEquals(
      Display.default.update(Pixel.lit, 10).toString,
      """..........#.............................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
  }
  test("draw signal") {
    assertEquals(
      Display.default.draw(Signal(Cycle(2), Register(value = 1))).toString,
      """.#......................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    assertEquals(
      Display.default.draw(Signal(Cycle(5), Register(value = 1))).toString,
      """........................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
  }
  test("part2") {
    assertEquals(
      Day10.part2(input),
      """##..##..##..##..##..##..##..##..##..##..
        |###...###...###...###...###...###...###.
        |####....####....####....####....####....
        |#####.....#####.....#####.....#####.....
        |######......######......######......####
        |#######.......#######.......#######.....""".stripMargin
    )
  }
  def testSignal(
      executions: List[Execution],
      cycle: Int,
      register: Int,
      display: Display,
      expectedOutput: String
  ): Display = {
    val signal = Day10.signal(Cycle(cycle), executions)
    assertEquals(signal, Signal(Cycle(cycle), Register(register)))
    val drawn = display.draw(signal)
    assertEquals(drawn.toString, expectedOutput)
    drawn
  }

  test("test signal") {
    val executions = Day10.executions(input)
    var display    = Display.default
    display = testSignal(
      executions,
      1,
      1,
      display,
      """#.......................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      2,
      1,
      display,
      """##......................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      3,
      16,
      display,
      """##......................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      4,
      16,
      display,
      """##......................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      5,
      5,
      display,
      """##..#...................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      6,
      5,
      display,
      """##..##..................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      7,
      11,
      display,
      """##..##..................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      8,
      11,
      display,
      """##..##..................................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      9,
      8,
      display,
      """##..##..#...............................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      10,
      8,
      display,
      """##..##..##..............................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      11,
      13,
      display,
      """##..##..##..............................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      12,
      13,
      display,
      """##..##..##..............................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      13,
      12,
      display,
      """##..##..##..#...........................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      14,
      12,
      display,
      """##..##..##..##..........................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      15,
      4,
      display,
      """##..##..##..##..........................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      16,
      4,
      display,
      """##..##..##..##..........................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      17,
      17,
      display,
      """##..##..##..##..#.......................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      18,
      17,
      display,
      """##..##..##..##..##......................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      19,
      21,
      display,
      """##..##..##..##..##......................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      20,
      21,
      display,
      """##..##..##..##..##......................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      21,
      21,
      display,
      """##..##..##..##..##..#...................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
    display = testSignal(
      executions,
      22,
      20,
      display,
      """##..##..##..##..##..##..................
        |........................................
        |........................................
        |........................................
        |........................................
        |........................................""".stripMargin
    )
  }
}
