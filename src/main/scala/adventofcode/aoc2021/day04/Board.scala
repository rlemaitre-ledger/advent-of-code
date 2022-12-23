package adventofcode.aoc2021.day04

import adventofcode.aoc2021.day04.Board.*
import adventofcode.utils.coordinates.Coordinates

final case class Board(numbers: Map[Int, Cell], lastDraw: Int = -1, drawCount: Int = 0) {
  private def columns: Seq[Iterable[Cell]] = (0 until 5).map { i =>
    numbers.filter((_, cell) => cell.coordinates.x == i).values.toList.sortBy(_.coordinates.y)
  }.toList
  private def rows: Seq[Iterable[Cell]] = (0 until 5).map { i =>
    numbers.filter((_, cell) => cell.coordinates.y == i).values.toList.sortBy(_.coordinates.x)
  }.toList
  def wins: Boolean = columns.exists(_.forall(_.drawn)) || rows.exists(_.forall(_.drawn))
  def score: Int    = numbers.values.filterNot(_.drawn).map(_.value).sum * lastDraw
  def draw(number: Int): Board =
    copy(numbers = numbers + (number -> numbers(number).mark), lastDraw = number, drawCount = drawCount + 1)
}
object Board {
  final case class Cell(value: Int, coordinates: Coordinates, drawn: Boolean = false) {
    def mark: Cell = copy(drawn = true)
  }
  def parseLines(lines: List[String]): List[Board] =
    def parseLine(line: String, index: Int): List[Cell] =
      println(s"parse line $index ($line)")
      line.split("\\W+").dropWhile(_.isEmpty).zipWithIndex.map((n, i) => Cell(n.toInt, Coordinates(i, index))).toList
    lines
      .filterNot(_.isEmpty)
      .grouped(5)
      .map { grid =>
        Board(
          (
            parseLine(grid(0), 0) ++
              parseLine(grid(1), 1) ++
              parseLine(grid(2), 2) ++
              parseLine(grid(3), 3) ++
              parseLine(grid(4), 4)
          ).map(cell => cell.value -> cell).toMap
        )
      }
      .toList
}
