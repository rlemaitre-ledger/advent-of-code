package adventofcode.aoc2022.day16

import Day16.Position
import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.ShortestPaths

final case class Day16(input: ValveNetwork) extends Problem[ValveNetwork, Int, Int](2022, 16, "Proboscidea Volcanium") {
  override def part1: Int = maxPressure(input, 30, 0)
  override def part2: Int = maxPressure(input, 26, 26)

  def maxPressure(network: ValveNetwork, time: Int, elephantTime: Int): Int = {
    val cache = collection.mutable.Map[Set[Valve], Int]().withDefaultValue(-1)

    def step(todo: Set[Valve], current: Position, elephant: Position, pressure: Int): Unit = {
      if cache(todo) >= pressure then return
      else cache(todo) = pressure

      for next <- todo do {
        val remaining = current.time - network.distances(current.valve)(next)
        if (remaining > 0) {
          val extra = remaining * next.pressureRate
          step(todo - next, Position(next, remaining), elephant, pressure + extra)
        }
      }
      for next <- todo do {
        val remaining = elephant.time - network.distances(elephant.valve)(next)
        if (remaining > 0) {
          val extra = remaining * next.pressureRate
          step(todo - next, current, Position(next, remaining), pressure + extra)
        }
      }
    }
    step(network.interesting, Position(network.start, time), Position(network.start, elephantTime), 0)
    cache.values.max
  }

}
object Day16 {
  val instance: Day16 = Day16(ValveNetwork.parse(inputLines("2022/day16.txt")))
  case class Position(valve: Valve, time: Int)
}
