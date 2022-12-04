package adventofcode.day01

import adventofcode.AdventOfCodeBase
import adventofcode.day01.Day1.elves
import scala.io.Source

object Day1 extends AdventOfCodeBase("day1.txt") {

  def elves(lines: List[String]): List[Elf] =
    lines
      .foldLeft(List.empty[Elf]) { case (acc, line) =>
        acc match
          case head :: next =>
            if (line.trim.isEmpty) {
              head.next :: acc
            } else {
              head.addCalories(line.toLong) :: next
            }
          case Nil =>
            if (line.trim.isEmpty) {
              Elf(0, 0L) :: acc
            } else {
              Elf(0, line.toLong) :: acc
            }
      }
      .reverse

  def maxCalories(elves: List[Elf]): Long =
    elves.maxBy(_.calories).calories

  def top3(elves: List[Elf]): List[Elf] =
    elves.sortBy(_.calories).reverse.take(3)

  def part1(input: List[String]): Long = maxCalories(elves(input))

  def part2(input: List[String]): Long = top3(elves(input)).map(_.calories).sum

  def main(args: Array[String]): Unit = {
    println(s"part1: ${part1(input)}")
    println(s"part2: ${part2(input)}")
  }
}

case class Elf(number: Int, calories: Long) {
  def next: Elf = Elf(number + 1, 0L)

  def addCalories(cal: Long): Elf = copy(calories = calories + cal)
}
