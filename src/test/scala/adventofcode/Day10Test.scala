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
    assertEquals(Day10(instructions(simple)).registerAt(Cycle(1)), Register(1))
    assertEquals(Day10(instructions(simple)).registerAt(Cycle(2)), Register(1))
    assertEquals(Day10(instructions(simple)).registerAt(Cycle(3)), Register(1))
    assertEquals(Day10(instructions(simple)).registerAt(Cycle(4)), Register(4))
    assertEquals(Day10(instructions(simple)).registerAt(Cycle(5)), Register(4))
    assertEquals(Day10(instructions(simple)).registerAt(Cycle(6)), Register(-1))
  }
  test("Register") {
    val instance = Day10(instructions(input))
    assertEquals(instance.registerAt(Cycle(20)), Register(21))
    assertEquals(instance.registerAt(Cycle(60)), Register(19))
    assertEquals(instance.registerAt(Cycle(100)), Register(18))
    assertEquals(instance.registerAt(Cycle(140)), Register(21))
    assertEquals(instance.registerAt(Cycle(180)), Register(16))
    assertEquals(instance.registerAt(Cycle(220)), Register(18))
  }
  test("Signal Strength") {
    val instance = Day10(instructions(input))
    assertEquals(instance.signal(Cycle(20)).strength, 420)
    assertEquals(instance.signal(Cycle(60)).strength, 1140)
    assertEquals(instance.signal(Cycle(100)).strength, 1800)
    assertEquals(instance.signal(Cycle(140)).strength, 2940)
    assertEquals(instance.signal(Cycle(180)).strength, 2880)
    assertEquals(instance.signal(Cycle(220)).strength, 3960)
  }
  test("part 1") {
    assertEquals(Day10(instructions(input)).part1, 13140)
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
    val instance = Day10(instructions(input))
    assertEquals(
      instance.part2,
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
    val instance = Day10(instructions(input))
    val signal   = instance.signal(Cycle(cycle))
    assertEquals(signal, Signal(Cycle(cycle), Register(register)))
    val drawn = display.draw(signal)
    assertEquals(drawn.toString, expectedOutput)
    drawn
  }

  test("test signal") {
    val instance   = Day10(instructions(input))
    val executions = instance.executions
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
  test("answers") {
    assertEquals(Day10.instance.run(Mode.Part1), 13480)
    assertEquals(
      Day10.instance.run(Mode.Part2),
      """####..##....##.###...##...##..####.#..#.
        |#....#..#....#.#..#.#..#.#..#.#....#.#..
        |###..#.......#.###..#....#....###..##...
        |#....#.##....#.#..#.#.##.#....#....#.#..
        |#....#..#.#..#.#..#.#..#.#..#.#....#.#..
        |####..###..##..###...###..##..#....#..#.""".stripMargin
    )
  }
}
