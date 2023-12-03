package adventofcode.aoc2023.day02
import adventofcode.Problem
import adventofcode.aoc2023.day02.Day02.*
import adventofcode.inputLines
import scala.util.matching.Regex

final case class Day02(input: List[Game]) extends Problem[List[Game], Int, Long](2023, 2, "Cube Conundrum"):
  override def part1: Int  = input.filter(_.isValid(Bag.part1)).map(_.id).sum
  override def part2: Long = input.map(_.minBag).map(_.power).sum
object Day02:
  val instance: Day02 = Day02(inputLines("2023/day02.txt").map(Game.parse))

  enum Cube(number: Int):
    case Red(number: Int)   extends Cube(number)
    case Green(number: Int) extends Cube(number)
    case Blue(number: Int)  extends Cube(number)
  final case class Game(id: Int, rounds: List[Round]):
    def isValid(bag: Bag): Boolean = rounds.forall(_.isValid(bag))
    def minBag: Bag =
      rounds.foldLeft(Bag.empty): (bag, round) =>
        bag.maxRed(round.red).maxBlue(round.blue).maxGreen(round.green)

  object Game:
    def parse(line: String): Game =
      val parts  = line.split(':')
      val id     = parts.head.split(' ').last.trim.toInt
      val rounds = parts.last.split("; ")
      Game(id, rounds.map(Round.parse).toList)

  final case class Bag(red: Int, green: Int, blue: Int):
    def maxRed(n: Int): Bag   = if n >= red then copy(red = n) else this
    def maxBlue(n: Int): Bag  = if n >= blue then copy(blue = n) else this
    def maxGreen(n: Int): Bag = if n >= green then copy(green = n) else this
    val power: Long           = red * green * blue

  object Bag:
    val part1: Bag = Bag(12, 13, 14)
    val empty: Bag = Bag(0, 0, 0)
  final case class Round(red: Int, green: Int, blue: Int):
    def isValid(bag: Bag): Boolean = bag.red >= red && bag.green >= green && bag.blue >= blue
  object Round:
    private def extract(line: String, color: String): Int =
      val parts = line.split(", ").map(_.trim)
      parts.find(_.contains(color)) match
        case Some(value) =>
          val strings = value.split(' ')
          strings.headOption.map(_.toInt).getOrElse(0)
        case None => 0

    def parse(line: String): Round =
      Round(extract(line, "red"), extract(line, "green"), extract(line, "blue"))
