package adventofcode

import Day04.*
import adventofcode.Day04.pairs
case class Day04(input: List[Pair]) extends AdventOfCodeBase[List[Pair], Int, Int] {
  override def part1: Int = input.count(_.assignmentsIncluded)
  override def part2: Int = input.count(_.overlaps)
}
object Day04 {
  val instance: Day04                        = Day04(pairs(inputLines("day04.txt")))
  def pairs(lines: List[String]): List[Pair] = lines.map(Pair.from)
  case class Assignment(start: Int, end: Int) {
    val elements: Set[Int]                     = (start to end).toSet
    def includes(other: Assignment): Boolean   = this.intersect(other) == other.elements
    def intersect(other: Assignment): Set[Int] = this.elements.intersect(other.elements)
  }
  object Assignment {
    def from(input: String): Assignment =
      val parts = input.split('-')
      Assignment(parts.head.toInt, parts.last.toInt)
  }
  case class Pair(first: Assignment, second: Assignment) {
    def assignmentsIncluded: Boolean = first.includes(second) || second.includes(first)
    def overlaps: Boolean            = first.intersect(second).nonEmpty
  }
  object Pair {
    def from(input: String): Pair =
      val parts = input.split(',')
      Pair(Assignment.from(parts.head), Assignment.from(parts.last))
  }
}
