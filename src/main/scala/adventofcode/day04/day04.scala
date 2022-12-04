package adventofcode.day04

import adventofcode.AdventOfCodeBase

object Day04 extends AdventOfCodeBase("day4.txt") {
  def pairs(lines: List[String]): List[Pair] = lines.map(Pair.from)

  def part1(lines: List[String]): Int =
    pairs(lines)
      .count(_.assignmentsIncluded)

  def main(args: Array[String]): Unit = {
    println(s"part1: ${part1(input)}")
  }
}

case class Assignment(start: Int, end: Int) {
  val elements: Set[Int] = (start to end).toSet

  def includes(other: Assignment): Boolean = this.elements.intersect(other.elements) == other.elements
}
object Assignment {
  def from(input: String): Assignment = {
    val parts = input.split('-')
    Assignment(parts.head.toInt, parts.last.toInt)
  }
}

case class Pair(first: Assignment, second: Assignment) {
  def assignmentsIncluded: Boolean = first.includes(second) || second.includes(first)
}
object Pair {
  def from(input: String): Pair = {
    val parts = input.split(',')
    Pair(Assignment.from(parts.head), Assignment.from(parts.last))
  }
}
