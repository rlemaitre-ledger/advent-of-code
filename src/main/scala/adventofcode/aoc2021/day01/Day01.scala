package adventofcode.aoc2021.day01
import adventofcode.Problem
import adventofcode.inputLines

final case class Day01(input: List[String]) extends Problem[List[String], Int, Int](2021, 01, "") {
  override def part1: Int = sliding(2).map(depths => depths.last - depths.head).count(_ > 0)
  override def part2: Int = sliding(3).map(_.sum).sliding(2).map(depths => depths.last - depths.head).count(_ > 0)
  private def sliding(i: Int) = input.map(_.toInt).sliding(i)
}
object Day01 {
  val instance: Day01 = Day01(inputLines("2021/day01.txt"))
}
