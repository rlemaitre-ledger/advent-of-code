package adventofcode

import adventofcode.Day01.Elf

case class Day01(input: List[Elf]) extends AdventOfCodeBase[List[Elf], Int, Int] {
  def maxCalories: Int = input.maxBy(_.calories).calories
  def top3: List[Elf]  = input.sortBy(_.calories).reverse.take(3)
  def part1: Int       = maxCalories
  def part2: Int       = top3.map(_.calories).sum
}
object Day01 {
  val instance: Day01 = Day01(elves(inputLines("day01.txt")))
  case class Elf(number: Int, calories: Int) {
    def next: Elf                  = Elf(number + 1, 0)
    def addCalories(cal: Int): Elf = copy(calories = calories + cal)
  }
  def elves(lines: List[String]): List[Elf] =
    lines
      .foldLeft(List.empty[Elf]) { case (acc, line) =>
        acc match
          case head :: next =>
            if (line.trim.isEmpty) head.next :: acc else head.addCalories(line.toInt) :: next
          case Nil =>
            if (line.trim.isEmpty) Elf(0, 0) :: acc else Elf(0, line.toInt) :: acc
      }
      .reverse
}
