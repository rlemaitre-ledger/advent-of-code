package adventofcode

import adventofcode.Mode.Part1
import adventofcode.Mode.Part2
import scala.io.Source

trait AdventOfCodeBase[T1, T2](path: String) {
  def input: List[String] = Source
    .fromResource(path)
    .getLines()
    .toList
  def part1(lines: List[String]): T1
  def part2(lines: List[String]): T2
  def run(mode: Mode): T1 | T2 = mode match
    case Part1 => part1(input)
    case Part2 => part2(input)
}
