package adventofcode.aoc2022.day24
import adventofcode.Problem
import adventofcode.inputLines

final case class Day24(input: Trip) extends Problem[Trip, Int, Int](2022, 24, "Blizzard Basin") {
  override def part1: Int = Iterator.iterate(input)(_.next).dropWhile(!_.atEnd).next().minutes
  override def part2: Int = ???
}
object Day24 {
  val instance: Day24 = Day24(Trip.parse(inputLines("2022/day24.txt")))
}
