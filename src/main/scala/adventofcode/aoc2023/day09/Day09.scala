package adventofcode.aoc2023.day09
import adventofcode.Problem
import adventofcode.aoc2023.day09.Day09.Oasis
import adventofcode.inputLines
import scala.annotation.tailrec
import scala.annotation.targetName

final case class Day09(input: List[String]) extends Problem[List[String], Int, Int](2023, 9, "Mirage Maintenance"):
  override def part1: Int = Oasis.parse(input).part1
  override def part2: Int = Oasis.parse(input).part2

object Day09:
  val instance: Day09 = Day09(inputLines("2023/day09.txt"))

  case class Oasis(lines: List[Line]):
    def part1: Int = lines.map(_.nextValue).sum
    def part2: Int = lines.map(_.previousValue).sum
  object Oasis:
    def parse(lines: List[String]): Oasis = Oasis(lines.map(Line.parse))

  final case class Line(values: List[Int]):
    def containsOnlyZero: Boolean = values.forall(_ == 0)
    def difference: Line          = Line(values.sliding(2).map(l => l.last - l.head).toList)

    override def toString: String = values.mkString(" ")
    def nextValue: Int            = nextLine.values.last
    def previousValue: Int        = nextLine.values.head

    private def nextLine: Line = {
      @tailrec
      def down(lines: List[Line]): List[Line] =
        if lines.head.containsOnlyZero then lines
        else down(lines.head.difference +: lines)

      @tailrec
      def up(lines: List[Line]): Line =
        lines match
          case Nil              => throw Exception("💥")
          case l :: Nil         => l
          case l1 :: l2 :: rest => up((l2 + l1) +: rest)

      val future = up(down(List(this)))
      future
    }

    @targetName("plusLine")
    def +(line: Line): Line =
      copy(values = (values.head - line.values.head) +: values :+ (values.last + line.values.last))

  object Line:
    def parse(s: String): Line = Line(s.split(" ").toList.map(_.toInt))
