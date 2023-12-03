package adventofcode.aoc2023.day03
import adventofcode.Problem
import adventofcode.inputLines

final case class Day03(input: Board) extends Problem[Board, Int, Long](2023, 3, "Gear Ratios"):
  override def part1: Int = input.validPartNumbers.map(_.value).sum
  override def part2: Long = input.gears.map(_.power).sum

object Day03:
  val instance: Day03 = Day03(Board.parse(inputLines("2023/day03.txt")))
