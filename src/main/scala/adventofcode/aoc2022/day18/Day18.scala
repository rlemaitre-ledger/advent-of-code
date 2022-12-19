package adventofcode.aoc2022.day18

import Day18.*
import adventofcode.Problem
import adventofcode.inputLines

final case class Day18(input: Set[Coordinates3D])
    extends Problem[Set[Coordinates3D], Int, Int](2022, 18, "Boiling Boulders") {
  override def part1: Int = input.toSeq.map(cube => cube.visibleSides(input)).sum

  override def part2: Int = {
    val airCubes = Box.enclosing(input).outsideCubes
    input.toSeq.map(_.neighbours.count(airCubes.contains)).sum
  }
}
object Day18 {
  val instance: Day18 = Day18(Coordinates3D.fromLines(inputLines("2022/day18.txt")))
}
