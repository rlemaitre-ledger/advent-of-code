package adventofcode.aoc2023.day11
import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.coordinates.Coordinates
import scala.annotation.tailrec

final case class Day11(input: List[String]) extends Problem[List[String], Long, Long](2023, 11, "Cosmic Expansion"):
  private val universe = Universe.parse(input)
  override def part1: Long = universe.expand(2).shortestPaths
  override def part2: Long = universe.expand(1_000_000).shortestPaths

object Day11:
  val instance: Day11 = Day11(inputLines("2023/day11.txt"))

final case class ExpandedUniverse(galaxies: List[Coordinates]):
  def shortestPaths: Long =
    galaxies
      .tails
      .flatMap: t =>
        t.headOption.map: h =>
          t.tail.map: t2 =>
            h.manhattanDistance(t2).toLong
      .toList
      .flatten
      .sum
final case class Universe(
    cells: Map[Coordinates, Cell],
    emptyRows: Set[Int],
    emptyCols: Set[Int],
    maxCols: Int,
    maxRows: Int
):
  def expand(age: Int): ExpandedUniverse =
    val newGalaxies = cells
      .filter(_._2 == Cell.Galaxy)
      .map: (coord, cell) =>
        val newCoords = Coordinates(coord.x + (age - 1) * emptyCols.count(_ < coord.x), coord.y + (age - 1) * emptyRows.count(_ < coord.y))
        newCoords
    ExpandedUniverse(newGalaxies.toList)
object Universe:
  def parse(input: List[String]): Universe =
    val maxX = input.map(_.length).max - 1
    val maxY = input.length - 1
    @tailrec
    def impl(lines: List[String], cells: Map[Coordinates, Cell], emptyRows: Set[Int], emptyCols: Set[Int]): Universe =
      if lines.isEmpty then Universe(cells, emptyRows, emptyCols, maxX, maxY)
      else
        val (line, rest) = lines.head -> lines.tail
        val y            = rest.length
        val values: Seq[(Coordinates, Cell)] = line.zipWithIndex.map: (char, x) =>
          char match
            case '.' => Coordinates(x, y) -> Cell.Empty
            case '#' => Coordinates(x, y) -> Cell.Galaxy
        impl(
          rest,
          cells ++ values.toMap,
          emptyRows ++ (if values.forall(_._2 == Cell.Empty) then Set(y) else Set.empty),
          emptyCols intersect values.filter(_._2 == Cell.Empty).map(_._1.x).toSet
        )
    impl(input, Map.empty, Set.empty, (0 to maxX).toSet)
enum Cell:
  case Empty
  case Galaxy
