package adventofcode.aoc2021.day02
import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.coordinates.Coordinates

final case class Day02(input: List[Coordinates]) extends Problem[List[Coordinates], Int, Int](2021, 2, "Dive!") {
  override def part1: Int = {
    val sum = input.foldLeft(Coordinates.origin)(_ + _)
    sum.x * sum.y
  }
  override def part2: Int = {
    val sum = input
      .foldLeft((Coordinates.origin, 0)) {
        case ((total, aim), Coordinates(x, 0)) => (Coordinates(total.x + x, total.y + aim * x), aim)
        case ((total, aim), Coordinates(0, y)) => (total, aim + y)
      }
      ._1
    sum.x * sum.y
  }
}
object Day02 {
  val instance: Day02 = Day02(parse(inputLines("2021/day02.txt")))
  def parse(lines: List[String]): List[Coordinates] = lines.map {
    case s"forward $n" => Coordinates(n.toInt, 0)
    case s"up $n"      => Coordinates(0, -n.toInt)
    case s"down $n"    => Coordinates(0, n.toInt)
  }
}
