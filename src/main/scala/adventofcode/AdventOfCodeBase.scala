package adventofcode

import scala.io.Source

trait AdventOfCodeBase(path: String) {
  val input: List[String] = Source
    .fromResource(path)
    .getLines()
    .toList
}
