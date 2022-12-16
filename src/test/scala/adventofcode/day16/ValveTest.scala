package adventofcode.day16

import munit.FunSuite

class ValveTest extends FunSuite {
  test("parse valve") {
    assertEquals(
      Valve.parse("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"),
      Valve("AA", 0, Set("BB", "DD", "II"))
    )
    assertEquals(
      Valve.parse("Valve BB has flow rate=13; tunnels lead to valves CC, AA"),
      Valve("BB", 13, Set("AA", "CC"))
    )
    assertEquals(
      Valve.parse("Valve HH has flow rate=22; tunnel leads to valve GG"),
      Valve("HH", 22, Set("GG"))
    )
  }
}
