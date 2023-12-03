package adventofcode.aoc2023.day03

import adventofcode.aoc2023.day03.Board.*
import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Direction
import cats.data.NonEmptyList
import cats.syntax.all.*

final case class Board(partNumbers: List[PartNumber], symbols: List[Symbol], emptys: List[Empty]):
  def validPartNumbers: List[PartNumber] =
    partNumbers
      .filter: p =>
        p.coordinates.exists(c => symbols.exists(_.coordinates.adjacentTo(c)))
  def gears: List[Gear] = 
    symbols
      .mapFilter: s =>
        val parts = partNumbers.filter(_.coordinates.exists(_.adjacentTo(s.coordinates))).toSet
        if s.symbol == '*' && parts.size == 2 then Gear(parts).some else None
object Board:
  def parse(lines: List[String]): Board =
    val (partNumbers: List[PartNumber], symbols: List[Symbol], emptys: List[Empty]) =
      lines.zipWithIndex.foldLeft((List.empty[PartNumber], List.empty[Symbol], List.empty[Empty])):
        case (acc, (line, y)) =>
          line.zipWithIndex.foldLeft(acc):
            case ((ps, ss, es), (c, x)) =>
              val coordinates = Coordinates(x, y)
              c match
                case '.' => (ps, ss, es :+ Empty(coordinates))
                case d: Char if d.isDigit =>
                  ps match
                    case Nil => (ps :+ PartNumber(d.toString.toInt, coordinates.pure[NonEmptyList]), ss, es)
                    case l: List[PartNumber]
                        if l.last.coordinates.last.sameRow(coordinates) && l.last.coordinates.last.adjacentTo(coordinates) =>
                      (ps.init :+ l.last.add(d.toString.toInt), ss, es)
                    case _ => (ps :+ PartNumber(d.toString.toInt, coordinates.pure[NonEmptyList]), ss, es)
                case _ => (ps, ss :+ Symbol(c, coordinates), es)
    Board(partNumbers, symbols, emptys)

  final case class Empty(coordinates: Coordinates)
  final case class Symbol(symbol: Char, coordinates: Coordinates)
  final case class PartNumber(value: Int, coordinates: NonEmptyList[Coordinates]):
    def add(v: Int): PartNumber = copy(value = value * 10 + v, coordinates = coordinates :+ coordinates.last.move(Direction.Right))
  final case class Gear(partNumbers: Set[PartNumber]):
    val power: Long = partNumbers.map(_.value.toLong).product
