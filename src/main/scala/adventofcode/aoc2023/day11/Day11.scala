package adventofcode.aoc2023.day11
import adventofcode.Problem
import adventofcode.aoc2023.day11.Cell.Empty
import adventofcode.aoc2023.day11.Cell.Galaxy
import adventofcode.inputLines
import adventofcode.time
import adventofcode.utils.coordinates.Coordinates
import scala.annotation.tailrec

final case class Day11(input: List[String]) extends Problem[List[String], Long, Long](2023, 11, "Cosmic Expansion"):
  private val optimized: OptimizedUniverse = Universe.optimized(input)
  private val naive: Universe              = Universe.parse(input)

  override def part1: Long =
    println("Get parsed universes")
    time:
        println(s"${naive.galaxies.size} galaxies in optimized universe")
    time:
        println(s"${optimized.count} galaxies in optimized universe")
    println("Compute shortest paths")
    println("\tNaive")
    time:
        naive.expand(2).shortestPaths
    println("\tOptimized")
    time:
        optimized.shortestPaths(2)
  override def part2: Long =
    println("Compute shortest paths")
    println("\tNaive")
    time:
        naive.expand(1_000_000).shortestPaths
    println("\tOptimized")
    time:
        optimized.shortestPaths(1_000_000)

object Day11:
  lazy val instance: Day11 = Day11(inputLines("2023/day11.txt"))

final case class ExpandedUniverse(galaxies: List[Coordinates]):
  def shortestPaths: Long =
    galaxies.tails
      .flatMap: t =>
        t.headOption.map: h =>
          t.tail.map: t2 =>
            h.manhattanDistance(t2).toLong
      .toList
      .flatten
      .sum
final case class Universe(
    galaxies: List[Coordinates],
    rowsToExpand: Vector[Int],
    colsToExpand: Vector[Int]
):
  def expand(age: Int): ExpandedUniverse =
    val newGalaxies = galaxies
      .map: coord =>
        Coordinates(coord.x + (age - 1) * colsToExpand(coord.x), coord.y + (age - 1) * rowsToExpand(coord.y))
    ExpandedUniverse(newGalaxies)

final case class OptimizedUniverse(xs: List[Int], ys: List[Int], addToX: Vector[Int], addToY: Vector[Int]):
  val count: Int = xs.length
  def shortestPaths(age: Int): Long = {
    val sumX = sum(xs, age, addToX)
    val sumY = sum(ys, age, addToY)
    sumX + sumY
  }
  def sum(values: List[Int], age: Int, toAdd: Vector[Int]): Long =
    val expandedValues = values.map(x => x + (age - 1) * toAdd(x))
    val multipliers = Range(1 - count, count - 1, 2).inclusive.toList
    expandedValues.zip(multipliers).map(_.toLong * _).sum

object Universe:
  def optimized(lines: List[String]): OptimizedUniverse =
    val nbRows = lines.length
    val nbCols = lines.head.length
    @tailrec
    def impl(queue: List[String], coordinates: List[(Int, Int)]): List[(Int, Int)] =
      if queue.isEmpty then coordinates
      else
        val line :: rest = queue: @unchecked
        val y            = rest.length
        val points =
          line.zipWithIndex.flatMap:
            case ('#', x) => Some(x, y)
            case _        => None
        impl(rest, coordinates ++ points.toList)
    val coordinates = impl(lines, List.empty)
    val cols = coordinates.map(_._1).sorted
    val emptyLines  = toExpand(nbRows, (0 until nbRows).toSet -- cols.toSet)
    val rows = coordinates.map(_._2).sorted
    val emptyCols   = toExpand(nbRows, (0 until nbCols).toSet -- rows.toSet)
    OptimizedUniverse(cols, rows, emptyLines, emptyCols)

  def parse(input: List[String]): Universe =
    val maxY = input.length
    val maxX = input.head.length
    @tailrec
    def impl(
        lines: List[String],
        cells: List[Coordinates],
        emptyRows: Set[Int],
        emptyCols: Set[Int]
    ): (List[Coordinates], Set[Int], Set[Int]) =
      if lines.isEmpty then (cells, emptyRows, emptyCols)
      else
        val (line, rest) = lines.head -> lines.tail
        val y            = rest.length
        val values: Seq[(Coordinates, Cell)] = line.zipWithIndex.map: (char, x) =>
          char match
            case '.' => Coordinates(x, y) -> Empty
            case '#' => Coordinates(x, y) -> Galaxy
        val galaxies     = cells ++ values.filter(_._2 == Galaxy).map(_._1)
        val newEmptyRows = emptyRows ++ (if values.forall(_._2 == Empty) then Set(y) else Set.empty)
        val newEmptyCols = emptyCols intersect values.filter(_._2 == Empty).map(_._1.x).toSet
        impl(
          rest,
          galaxies,
          newEmptyRows,
          newEmptyCols
        )

    val (cells, emptyRows, emptyCols) = impl(input, List.empty, Set.empty, (0 until maxX).toSet)
    Universe(cells, toExpand(maxY, emptyRows), toExpand(maxX, emptyCols))
  private def toExpand(length: Int, indices: Set[Int]): Vector[Int] =
    indices.foldLeft(Vector.fill(length)(0)) { (v, y) =>
      v.splitAt(y) match
        case (before, after) =>
          before ++ after.map(_ + 1)
    }
enum Cell:
  case Empty
  case Galaxy
