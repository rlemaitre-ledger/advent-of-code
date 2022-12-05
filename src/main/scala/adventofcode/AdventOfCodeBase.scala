package adventofcode

import scala.io.Source

trait AdventOfCodeBase[T](path: String) {
  val input: List[String] = Source
    .fromResource(path)
    .getLines()
    .toList
  def part1(lines: List[String]): T
  def part2(lines: List[String]): T
}
