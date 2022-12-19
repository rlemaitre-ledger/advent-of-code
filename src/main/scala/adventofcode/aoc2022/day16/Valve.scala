package adventofcode.aoc2022.day16

final case class Valve(name: String, pressureRate: Int, tunnelsTo: Set[String], timeToOpen: Int = 1)
object Valve {
  def parse(line: String): Valve = line match
    case s"Valve ${name} has flow rate=${pressure}; tunnels lead to valves ${to}" =>
      Valve(name, pressure.toInt, to.split(',').map(_.trim).toSet)
    case s"Valve ${name} has flow rate=${pressure}; tunnel leads to valve ${to}" =>
      Valve(name, pressure.toInt, Set(to))
}
