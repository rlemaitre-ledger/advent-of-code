package adventofcode

import adventofcode.Mode.Part1
import adventofcode.Mode.Part2
import scala.io.Source

trait AdventOfCodeBase[I, T1, T2]:
  def input: I
  def part1: T1
  def part2: T2
  def run(mode: Mode): T1 | T2 = mode match
    case Part1 => part1
    case Part2 => part2
