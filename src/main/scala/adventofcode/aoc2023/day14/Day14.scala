package adventofcode.aoc2023.day14
import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.matrix.*
import scala.annotation.tailrec
import scala.collection.immutable

final case class Day14(input: List[String])
    extends Problem[List[String], Int, Int](2023, 14, "Parabolic Reflector Dish"):
  override def part1: Int = Platform.parse(input).tilt.load
  override def part2: Int =
    val number      = 1_000_000_000
    @tailrec
    def detectCycle(cache: Map[String, Int], loads: Map[Int, Int], platform: Platform, i: Int): Int =
      val cyclePos = cache.get(platform.show)
      if cyclePos.isDefined then
        val start = cyclePos.get
        val end = i - 1
        val cycleLength = end - start + 1
        val position    = ((number - start + 1) % cycleLength) + start - 1
        loads(position)
      else
        detectCycle(cache.updated(platform.show, i), loads.updated(i, platform.load), platform.cycle, i + 1)
    detectCycle(Map.empty, Map.empty,Platform.parse(input), 0)


object Day14:
  val instance: Day14 = Day14(inputLines("2023/day14.txt"))

case class Platform(rocks: Vector[Vector[Option[Rock]]], sizeX: Int, sizeY: Int):

  def show: String =
    rocks.reverse
      .map: line =>
        line
          .map: rock =>
            rock.map(_.show).getOrElse(".")
          .mkString("")
      .mkString("\n")
  private def maxFreeLine(minLine: Int, column: Int): Int =
    val minCube = (minLine until sizeY).find(rocks(_)(column).contains(Rock.Cube)).getOrElse(sizeY)
    (minLine until minCube).findLast(rocks(_)(column).isEmpty).getOrElse(minLine)
  def cycle: Platform =
    val north = tilt
    val west = north.rotate.tilt
    val south = west.rotate.tilt
    val east = south.rotate.tilt
    east.rotate
  def tilt: Platform =
    @tailrec
    def impl(p: Platform, line: Int): Platform =
      if line == -1 then p
      else
        val lineRocks = p.rocks(line)
        val moves = lineRocks.zipWithIndex
          .collect { case (Some(Rock.Rounded), x) => Coordinates(x, line) -> Coordinates(x, p.maxFreeLine(line, x)) }
          .filter(_ != _)
        val newRocks = moves.foldLeft(p.rocks):
          case (acc, (oldXY, newXY)) =>
            acc
              .updated(oldXY.y, acc(oldXY.y).updated(oldXY.x, None))
              .updated(newXY.y, acc(newXY.y).updated(oldXY.x, Some(Rock.Rounded)))
        val newPlatform = Platform(newRocks, p.sizeX, p.sizeY)
        impl(newPlatform, line - 1)
    impl(this, sizeY - 1)

  def rotate: Platform = Platform(rocks.rotateRight, sizeY, sizeX)

  def load: Int = rocks.zipWithIndex
    .map: (line, y) =>
      line.count(_.contains(Rock.Rounded)) * (y + 1)
    .sum

object Platform:
  def empty: Platform = Platform(Vector.empty, 0, 0)
  def parse(input: List[String]): Platform =
    val lineCount = input.size
    val colCount  = input.head.length
    val rocks = input.foldLeft(Vector.empty[Vector[Option[Rock]]]):
      case (acc, line) =>
        val value: Vector[Option[Rock]] = line.foldLeft(Vector.empty[Option[Rock]]):
          case (acc2, char) =>
            char match
              case '.' => acc2 :+ None
              case 'O' => acc2 :+ Some(Rock.Rounded)
              case '#' => acc2 :+ Some(Rock.Cube)
        acc.prepended(value)
    Platform(rocks, colCount, lineCount)

enum Rock:
  case Rounded
  case Cube
extension (rock: Rock)
  def show: String = rock match
    case Rock.Rounded => "O"
    case Rock.Cube    => "#"
  def toString: String = rock.show
