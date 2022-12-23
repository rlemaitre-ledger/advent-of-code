package adventofcode.aoc2022.day23
import adventofcode.Problem
import adventofcode.inputLines

final case class Day23(input: Field) extends Problem[Field, Int, Int](2022, 23, "Unstable Diffusion") {
  override def part1: Int = Iterator.iterate(input)(_.next).drop(10).next().score
  override def part2: Int = Iterator.iterate(input)(_.next).dropWhile(_.elvesMoved).next().round
}
object Day23 {
  val instance: Day23 = Day23(Field.parse(inputLines("2022/day23.txt")))
}
