package adventofcode

import scala.io.Source

trait AdventOfCodeBase[T1, T2](path: String) {
  val input: List[String] = Source
    .fromResource(path)
    .getLines()
    .toList
  def part1(lines: List[String]): T1
  def part2(lines: List[String]): T2
}
