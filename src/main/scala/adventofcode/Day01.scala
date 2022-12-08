package adventofcode

object Day01 extends AdventOfCodeBase[Int]("day01.txt") {

  def elves(lines: List[String]): List[Elf] =
    lines
      .foldLeft(List.empty[Elf]) { case (acc, line) =>
        acc match
          case head :: next =>
            if (line.trim.isEmpty) {
              head.next :: acc
            } else {
              head.addCalories(line.toInt) :: next
            }
          case Nil =>
            if (line.trim.isEmpty) {
              Elf(0, 0) :: acc
            } else {
              Elf(0, line.toInt) :: acc
            }
      }
      .reverse

  def maxCalories(elves: List[Elf]): Int =
    elves.maxBy(_.calories).calories

  def top3(elves: List[Elf]): List[Elf] =
    elves.sortBy(_.calories).reverse.take(3)

  def part1(input: List[String]): Int = maxCalories(elves(input))

  def part2(input: List[String]): Int = top3(elves(input)).map(_.calories).sum

  case class Elf(number: Int, calories: Int) {
    def next: Elf = Elf(number + 1, 0)

    def addCalories(cal: Int): Elf = copy(calories = calories + cal)
  }

}
