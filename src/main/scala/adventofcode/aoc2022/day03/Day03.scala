package adventofcode.aoc2022.day03

import Day03.*
import adventofcode.Problem
import adventofcode.aoc2022.day03.Day03.Group
import adventofcode.inputLines

case class Day03(input: List[RugSack]) extends Problem[List[RugSack], Int, Int](2022, 3, "Rucksack Reorganization") {
  def groups: List[Group] = input.grouped(3).map(Group.apply).toList
  override def part1: Int = input.map(_.misplaced).map(_.priority).sum

  override def part2: Int = groups.map(_.badge).map(_.priority).sum
}
object Day03 {
  val instance: Day03 = Day03(inputLines("2022/day03.txt").map(RugSack.fromString))
  case class Group(elves: List[RugSack]) {
    val badge: Item =
      elves
        .foldLeft(List.empty[Item]) { (acc, sack) => acc ++ sack.distinctItems.toList }
        .groupBy(identity)
        .map { (k, v) => (k, v.size) }
        .filter { (_, size) => size == 3 }
        .map(_._1)
        .head
  }

  case class RugSack(first: Compartment, second: Compartment):
    def misplaced: Item =
      val inCommon = first.distinctItems.intersect(second.distinctItems)
      inCommon.head

    def distinctItems: Set[Item] = first.distinctItems ++ second.distinctItems

  object RugSack:
    def fromString(line: String): RugSack =
      RugSack(
        Compartment.fromString(line.substring(0, line.length / 2)),
        Compartment.fromString(line.substring(line.length / 2, line.length))
      )

  case class Compartment(items: List[Item]):
    def distinctItems: Set[Item] = items.toSet

  object Compartment:
    def fromString(str: String): Compartment = Compartment(str.toList.map(Item.fromChar))

  case class Item(name: Char, priority: Int)

  object Item:
    def fromChar(c: Char): Item = Item(c, priority(c))

    def priority(c: Char): Int = if (c.isLower) c.toInt - 'a' + 1 else c.toInt - 'A' + 27
}
