package adventofcode.day14

import adventofcode.Coordinates
import adventofcode.Direction
import adventofcode.Mode
import adventofcode.Problem
import adventofcode.day14.Day14.*
import adventofcode.inputLines
import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.collection.mutable.Set as MutableSet
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

case class Day14(input: Input) extends Problem[Input, Int, Int]("Regolith Reservoir") {
  override def part1: Int =
    val cavern = input.part1
    while (!cavern.fallsToInfinite) cavern.tick()
    cavern.sandAtRest.size

  override def part2: Int =
    val cavern = input.part2
    while (!cavern.isFull) cavern.tick()
    cavern.sandAtRest.size
}
object Day14 {
  val source: Coordinates = Coordinates(500, 0)
  val instance: Day14 = {
    val lines = inputLines("day14.txt")
    Day14(Input(cavern(lines, Mode.Part1), cavern(lines, Mode.Part2)))
  }

  case class Input(part1: Cavern, part2: Cavern)

  def paths(lines: List[String]): List[Path] = PathParser.parse(lines)

  def cavern(lines: List[String], mode: Mode): Cavern =
    Cavern(paths(lines).toSet.flatMap(_.wallCells), MutableSet.empty, source, mode)

  class Cavern(
      val rocks: Set[Coordinates],
      var sandAtRest: MutableSet[Coordinates],
      var current: Coordinates,
      val mode: Mode
  ) {
    val floorY: Int = rocks.map(_.y).max + 2

    def fallsToInfinite: Boolean = !(rocks ++ sandAtRest).exists(c => c.x == current.x && c.y >= current.y)

    def isFull: Boolean = sandAtRest.contains(source)

    def isAvailable(c: Coordinates): Boolean = mode match
      case Mode.Part1 => !rocks.contains(c) && !sandAtRest.contains(c)
      case Mode.Part2 => !rocks.contains(c) && !sandAtRest.contains(c) && c.y < floorY

    def tick(): Unit =
      val up      = current.move(Direction.Up)
      val upLeft  = up.move(Direction.Left)
      val upRight = up.move(Direction.Right)
      if (isAvailable(up)) current = up
      else if (isAvailable(upLeft)) current = upLeft
      else if (isAvailable(upRight)) current = upRight
      else
        sandAtRest.add(current): @nowarn
        current = source
  }
  final case class Path(edges: List[Coordinates]) {
    def wallCells: Set[Coordinates] = edges.sliding(2).flatMap(l => wall(l.head, l.last)).toSet

    def wall(from: Coordinates, to: Coordinates): List[Coordinates] =
      from.directionTo(to) match
        case Nil       => List(from, to)
        case head :: _ => from :: wall(from.move(head), to)
  }

  object PathParser extends JavaTokenParsers {
    override def skipWhitespace: Boolean = true

    private val int = wholeNumber

    def path: Parser[Path] = repsep(coordinates, "->") ^^ {
      Path.apply
    }

    def coordinates: Parser[Coordinates] = int ~ "," ~ int ^^ { case x ~ "," ~ y => Coordinates(x.toInt, y.toInt) }

    def parse(lines: List[String]): List[Path] = lines.map { l =>
      parseAll(path, l) match
        case Success(result, _) => result
        case _: NoSuccess       => throw new IllegalArgumentException()
    }
  }
}
