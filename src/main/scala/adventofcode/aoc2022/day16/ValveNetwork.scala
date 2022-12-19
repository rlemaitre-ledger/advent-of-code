package adventofcode.aoc2022.day16

import adventofcode.*
import scala.annotation.nowarn
import scala.collection.immutable.List
import scala.collection.mutable.Map as MutableMap

final case class ValveNetwork(valvesByName: Map[String, Valve], tunnels: Map[Valve, List[Tunnel]], start: Valve) {
  val valves: Set[Valve]                     = valvesByName.values.toSet
  val interesting: Set[Valve]                = valves.filterNot(_.pressureRate == 0)
  val adjacency: Map[Valve, Seq[Valve]]      = tunnels.map { case (v, tunnels) => v -> tunnels.map(_.to) }
  val distances: Map[Valve, Map[Valve, Int]] = valves.map(v => v -> bfs(v, adjacency.apply)).toMap
}
object ValveNetwork {
  def parse(lines: List[String]): ValveNetwork = {
    val valves: Map[String, Valve] = lines.map(Valve.parse).map(v => v.name -> v).toMap
    val tunnels: Map[Valve, List[Tunnel]] =
      valves.values.map(v => v -> v.tunnelsTo.map(name => Tunnel(v, valves(name), 1)).toList).toMap
    ValveNetwork(valves, tunnels, valves("AA"))
  }
}
