package adventofcode.day16

import adventofcode.AoCTest
import adventofcode.bfs
import adventofcode.time
import adventofcode.utils.ShortestPaths

class Day16Test extends AoCTest {
  override val lines: String = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                                 |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                                 |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                                 |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                                 |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                                 |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                                 |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                                 |Valve HH has flow rate=22; tunnel leads to valve GG
                                 |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                                 |Valve JJ has flow rate=21; tunnel leads to valve II
                                 |""".stripMargin
  val aa: Valve = Valve("AA", 0, Set("DD", "II", "BB"))
  val bb: Valve = Valve("BB", 13, Set("CC", "AA"))
  val cc: Valve = Valve("CC", 2, Set("DD", "BB"))
  val dd: Valve = Valve("DD", 20, Set("CC", "AA", "EE"))
  val ee: Valve = Valve("EE", 3, Set("FF", "DD"))
  val ff: Valve = Valve("FF", 0, Set("EE", "GG"))
  val gg: Valve = Valve("GG", 0, Set("FF", "HH"))
  val hh: Valve = Valve("HH", 22, Set("GG"))
  val ii: Valve = Valve("II", 0, Set("AA", "JJ"))
  val jj: Valve = Valve("JJ", 21, Set("II"))
  val valves: Map[String, Valve] = Map(
    "AA" -> aa,
    "BB" -> bb,
    "CC" -> cc,
    "DD" -> dd,
    "EE" -> ee,
    "FF" -> ff,
    "GG" -> gg,
    "HH" -> hh,
    "II" -> ii,
    "JJ" -> jj
  )
  private val tunnels: Map[Valve, List[Tunnel]] = Map(
    aa -> List(Tunnel(aa, dd, 1), Tunnel(aa, ii, 1), Tunnel(aa, bb, 1)),
    bb -> List(Tunnel(bb, cc, 1), Tunnel(bb, aa, 1)),
    cc -> List(Tunnel(cc, dd, 1), Tunnel(cc, bb, 1)),
    dd -> List(Tunnel(dd, cc, 1), Tunnel(dd, aa, 1), Tunnel(dd, ee, 1)),
    ee -> List(Tunnel(ee, ff, 1), Tunnel(ee, dd, 1)),
    ff -> List(Tunnel(ff, ee, 1), Tunnel(ff, gg, 1)),
    gg -> List(Tunnel(gg, ff, 1), Tunnel(gg, hh, 1)),
    hh -> List(Tunnel(hh, gg, 1)),
    ii -> List(Tunnel(ii, aa, 1), Tunnel(ii, jj, 1)),
    jj -> List(Tunnel(jj, ii, 1))
  )
  val network: ValveNetwork = ValveNetwork(valves, tunnels, aa)
  val testInstance: Day16   = Day16(network)
  test("parse input") {
    assertEquals(ValveNetwork.parse(input), network)
  }
  test("part 1") {
    assertEquals(testInstance.part1, 1651)
  }
  test("part 2") {
    assertEquals(testInstance.part2, 1707)
  }
  test("bfs") {
    assertEquals(
      bfs(aa, tunnels.map { case (k, v) => (k, v.map(_.to).sortBy(_.name)) }),
      Map(
        aa -> 1,
        ii -> 2,
        dd -> 2,
        bb -> 2,
        ee -> 3,
        cc -> 3,
        jj -> 3,
        ff -> 4,
        gg -> 5,
        hh -> 6
      )
    )
  }
  test("lowest cost") {
    val shortestPaths = ShortestPaths.from(aa, network.tunnels)
    assertEquals(shortestPaths.cost(bb), 1)
    assertEquals(shortestPaths.cost(cc), 2)
    assertEquals(shortestPaths.cost(dd), 1)
    assertEquals(shortestPaths.cost(ee), 2)
    assertEquals(shortestPaths.cost(ff), 3)
    assertEquals(shortestPaths.cost(gg), 4)
    assertEquals(shortestPaths.cost(hh), 5)
    assertEquals(shortestPaths.cost(ii), 1)
    assertEquals(shortestPaths.cost(jj), 2)
  }
  test("shortest path") {
    val shortestPaths = ShortestPaths.from(aa, network.tunnels)
    assertEquals(ShortestPaths.from(cc, network.tunnels.removed(cc)).pathTo(aa), Nil)
    assertEquals(shortestPaths.pathTo(aa), Nil)
    assertEquals(shortestPaths.pathTo(bb), List(Tunnel(aa, bb, 1)))
    assertEquals(shortestPaths.pathTo(cc), List(Tunnel(aa, dd, 1), Tunnel(dd, cc, 1)))
    assertEquals(shortestPaths.pathTo(dd), List(Tunnel(aa, dd, 1)))
    assertEquals(shortestPaths.pathTo(ee), List(Tunnel(aa, dd, 1), Tunnel(dd, ee, 1)))
    assertEquals(shortestPaths.pathTo(ff), List(Tunnel(aa, dd, 1), Tunnel(dd, ee, 1), Tunnel(ee, ff, 1)))
    assertEquals(
      shortestPaths.pathTo(gg),
      List(Tunnel(aa, dd, 1), Tunnel(dd, ee, 1), Tunnel(ee, ff, 1), Tunnel(ff, gg, 1))
    )
    assertEquals(
      shortestPaths.pathTo(hh),
      List(Tunnel(aa, dd, 1), Tunnel(dd, ee, 1), Tunnel(ee, ff, 1), Tunnel(ff, gg, 1), Tunnel(gg, hh, 1))
    )
    assertEquals(shortestPaths.pathTo(ii), List(Tunnel(aa, ii, 1)))
    assertEquals(shortestPaths.pathTo(jj), List(Tunnel(aa, ii, 1), Tunnel(ii, jj, 1)))
  }
  test("time does not change anything") {
    assertEquals(time { 2 + 2 }, 4)
  }
}
