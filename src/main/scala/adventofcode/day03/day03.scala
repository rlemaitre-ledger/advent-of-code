package adventofcode.day03

import adventofcode.AdventOfCodeBase

object Day03 extends AdventOfCodeBase("day03.txt") {
  def totalPriority(lines: List[String]): Int =
    lines
      .map(RugSack.fromString)
      .map(_.misplaced)
      .map(_.priority)
      .sum

  def groups(lines: List[String]): List[Group] =
    lines
      .map(RugSack.fromString)
      .grouped(3)
      .map(Group.apply)
      .toList

  def priority(groups: List[Group]): Int =
    groups.map(_.badge).map(_.priority).sum

  override def part1(lines: List[String]): Int = totalPriority(lines)

  override def part2(lines: List[String]): Int = priority(groups(lines))
}
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

case class RugSack(first: Compartment, second: Compartment) {
  def misplaced: Item = {
    val inCommon = first.distinctItems.intersect(second.distinctItems)
    inCommon.head
  }
  def distinctItems: Set[Item] = first.distinctItems ++ second.distinctItems
}
object RugSack {
  def fromString(line: String): RugSack =
    RugSack(
      Compartment.fromString(line.substring(0, line.length / 2)),
      Compartment.fromString(line.substring(line.length / 2, line.length))
    )
}

case class Compartment(items: List[Item]) {
  def distinctItems: Set[Item] = items.toSet
}

object Compartment {
  def fromString(str: String): Compartment =
    Compartment(str.toList.map(Item.fromChar))
}
case class Item(name: Char, priority: Int)

object Item {
  def fromChar(c: Char): Item =
    Item(c, priority(c))

  def priority(c: Char): Int = {
    if (c.isLower) {
      c.toInt - 'a' + 1
    } else {
      c.toInt - 'A' + 27
    }
  }
}
