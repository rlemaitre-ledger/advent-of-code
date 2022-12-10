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
    assertEquals(Day10.signal(Cycle(20), executions), Signal(420))
    assertEquals(Day10.signal(Cycle(60), executions), Signal(1140))
    assertEquals(Day10.signal(Cycle(100), executions), Signal(1800))
    assertEquals(Day10.signal(Cycle(140), executions), Signal(2940))
    assertEquals(Day10.signal(Cycle(180), executions), Signal(2880))
    assertEquals(Day10.signal(Cycle(220), executions), Signal(3960))
  }
  test("part 1") {
    assertEquals(Day10.part1(input), 13140)
  }
}
