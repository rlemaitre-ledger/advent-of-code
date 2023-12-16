package adventofcode.aoc2023.day16
import adventofcode.Problem
import adventofcode.aoc2023.day16.Tile.*
import adventofcode.inputLines
import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Direction
import adventofcode.utils.coordinates.Direction.*
import scala.annotation.tailrec
final case class Day16(input: List[String]) extends Problem[List[String], Int, Int](2023, 16, "The Floor Will Be Lava"):
  val height: Int         = input.size
  val width: Int          = input.head.length
  override def part1: Int = Cave.parse(input).energy(Coordinates(0, height - 1), Right)
  override def part2: Int = Cave.parse(input).startingTiles.map(Cave.parse(input).energy).max
object Day16:
  val instance: Day16 = Day16(inputLines("2023/day16.txt"))
final case class Cave(tiles: Map[Coordinates, Tile], width: Int, height: Int):
  private def nextTiles(coordinates: Coordinates, direction: Direction): Set[(Coordinates, Direction)] =
    tiles(coordinates).nextTiles(coordinates, direction).filter((c, _) => tiles.contains(c))

  private val borders = (0 until width).flatMap: x =>
    Set(Coordinates(x, 0), Coordinates(x, height - 1)) ++
      (0 until (height - 1)).flatMap: y =>
        Set(Coordinates(0, y), Coordinates(width - 1, y))

  val startingTiles: Seq[(Coordinates, Direction)] = borders.flatMap:
    case coordinates @ Coordinates(0, 0)                         => Set(coordinates -> Up, coordinates -> Right)
    case coordinates @ Coordinates(0, y) if y == this.height - 1 => Set(coordinates -> Down, coordinates -> Right)
    case coordinates @ Coordinates(x, 0) if x == this.width - 1  => Set(coordinates -> Down, coordinates -> Right)
    case coordinates @ Coordinates(x, y) if x == this.width - 1 && y == this.height - 1 =>
      Set(coordinates -> Down, coordinates -> Right)
    case coordinates @ Coordinates(x, 0)                         => Set(coordinates -> Up)
    case coordinates @ Coordinates(x, y) if y == this.height - 1 => Set(coordinates -> Down)
    case coordinates @ Coordinates(0, x)                         => Set(coordinates -> Right)
    case coordinates @ Coordinates(x, y) if x == this.width - 1  => Set(coordinates -> Left)
    case _                                                       => Set.empty

  def show[T](matrix: Map[Coordinates, T])(f: (Coordinates, T) => String): String =
    val lines = (0 until height).map: y =>
      (0 until width)
        .map: x =>
          val coordinates = Coordinates(x, y)
          matrix.get(coordinates).map(f(coordinates, _)).getOrElse('.')
        .mkString
    lines.reverse.mkString("\n")

  def energy(start: Coordinates, direction: Direction): Int =
    @tailrec
    def impl(
        queue: List[(Coordinates, Direction)],
        seen: Set[(Coordinates, Direction)]
    ): Set[(Coordinates, Direction)] =
      if queue.isEmpty then seen
      else
        val (current, direction) :: rest = queue: @unchecked
        if seen.contains((current, direction)) then impl(rest, seen)
        else impl(rest ++ nextTiles(current, direction), seen + ((current, direction)))

    val value = impl(List((start, direction)), Set.empty).toMap
    value.size
object Cave:
  def parse(lines: List[String]): Cave =
    val height = lines.size
    val width  = lines.head.length
    val tiles = lines.zipWithIndex
      .flatMap: (line, y) =>
        line.zipWithIndex.map: (char, x) =>
          Coordinates(x, width - y - 1) -> Tile(char)
      .toMap
    Cave(tiles, width, height)

enum Tile(val char: Char):
  case Mirror             extends Tile('/')
  case BackMirror         extends Tile('\\')
  case HorizontalSplitter extends Tile('-')
  case VerticalSplitter   extends Tile('|')
  case Empty              extends Tile('.')

extension (tile: Tile)
  def isMirror: Boolean             = tile == Mirror || tile == BackMirror
  def isSplitter: Boolean           = tile == HorizontalSplitter || tile == VerticalSplitter
  def isHorizontalSplitter: Boolean = tile == HorizontalSplitter
  def isVerticalSplitter: Boolean   = tile == VerticalSplitter
  def isEmpty: Boolean              = tile == Empty
  def nextTiles(coordinates: Coordinates, direction: Direction): Set[(Coordinates, Direction)] =
    (direction, tile) match
      case (Up, Mirror)                => Set(coordinates.right -> Right)
      case (Down, Mirror)              => Set(coordinates.left -> Left)
      case (Left, Mirror)              => Set(coordinates.down -> Down)
      case (Right, Mirror)             => Set(coordinates.up -> Up)
      case (Up, BackMirror)            => Set(coordinates.left -> Left)
      case (Down, BackMirror)          => Set(coordinates.right -> Right)
      case (Left, BackMirror)          => Set(coordinates.up -> Up)
      case (Right, BackMirror)         => Set(coordinates.down -> Down)
      case (Up, HorizontalSplitter)    => Set(coordinates.left -> Left, coordinates.right -> Right)
      case (Down, HorizontalSplitter)  => Set(coordinates.left -> Left, coordinates.right -> Right)
      case (Left, HorizontalSplitter)  => Set(coordinates.left -> Left)
      case (Right, HorizontalSplitter) => Set(coordinates.right -> Right)
      case (Up, VerticalSplitter)      => Set(coordinates.up -> Up)
      case (Down, VerticalSplitter)    => Set(coordinates.down -> Down)
      case (Left, VerticalSplitter)    => Set(coordinates.up -> Up, coordinates.down -> Down)
      case (Right, VerticalSplitter)   => Set(coordinates.up -> Up, coordinates.down -> Down)
      case (Up, Empty)                 => Set(coordinates.up -> Up)
      case (Down, Empty)               => Set(coordinates.down -> Down)
      case (Left, Empty)               => Set(coordinates.left -> Left)
      case (Right, Empty)              => Set(coordinates.right -> Right)

object Tile:
  def apply(char: Char): Tile = char match
    case '/'  => Mirror
    case '\\' => BackMirror
    case '-'  => HorizontalSplitter
    case '|'  => VerticalSplitter
    case '.'  => Empty
    case _    => throw new IllegalArgumentException(s"Unknown tile: $char")
