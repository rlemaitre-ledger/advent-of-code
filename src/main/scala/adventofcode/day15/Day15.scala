package adventofcode.day15

import adventofcode.AdventOfCodeBase
import adventofcode.Coordinates
import adventofcode.day15.Day15.*
import adventofcode.inputLines
import scala.collection.immutable.TreeMap
import scala.util.parsing.combinator.JavaTokenParsers

case class Day15(input: List[Sensor], lineInPart1: Int, maxCols: Int)
    extends AdventOfCodeBase[List[Sensor], Int, Long] {
  override def part1: Int = positionWithoutBeacon(None)

  override def part2: Long = tuningFrequency
  def tuningFrequency: Long = {
    val list          = possibleSolutions(input, 0, maxCols)
    val (line, multi) = list.head
    multi.free.map(_ * 4000000L + line).getOrElse(-1)
  }
  def beaconsAtLine(line: Int): List[Coordinates] =
    input.map(_.closestBeacon.coordinates).filter(_.y == line)
  def sensorsAtLine(line: Int): List[Coordinates] =
    input.map(_.coordinates).filter(_.y == line)
  def positionWithoutBeacon(limits: Option[(Int, Int)]): Int = {
    val m: MultiRange = multiRange(input, lineInPart1, limits)
      .removeAll(beaconsAtLine(lineInPart1).map(_.x))
      .removeAll(sensorsAtLine(lineInPart1).map(_.x))
    println(m)
    m.size
  }

  private def multiRange(sensors: List[Sensor], line: Int, limits: Option[(Int, Int)]) = {
    val multiRange = sensors
      .map(_.inRangeAtLine(line))
      .foldLeft(MultiRange.empty(limits))((m, r) => m.add(r))
    multiRange
  }

  def possibleSolutions(sensors: List[Sensor], min: Int, max: Int): List[(Int, MultiRange)] =
    val limits = Some((min, max))
    (min to max)
      .map(i => {
        (i, multiRange(sensors, i, limits))
      })
      .filter(_._2.size != (1 + max - min))
      .toList

}
object Day15 {
  val instance: Day15                            = Day15(sensors(inputLines("day15.txt")), 2000000, 4000000)
  def sensors(lines: List[String]): List[Sensor] = lines.map(Sensor.parse)

  final case class Beacon(coordinates: Coordinates)

  final case class Sensor(coordinates: Coordinates, closestBeacon: Beacon) {
    val radius: Int = coordinates.manhattanDistance(closestBeacon.coordinates)

    def inRangeAtLine(line: Int): IntRange =
      coordinates.rangeWithinInLine(radius, line)
  }

  object Sensor {
    def parse(str: String): Sensor = str match {
      case s"Sensor at x=${sensorX}, y=${sensorY}: closest beacon is at x=${beaconX}, y=${beaconY}" =>
        Sensor(Coordinates(sensorX.toInt, sensorY.toInt), Beacon(Coordinates(beaconX.toInt, beaconY.toInt)))
    }
  }

  final case class IntRange(start: Int, end: Int) {
    def toRange: Range = start to end

    def includes(other: IntRange): Boolean = other.start >= start && other.end <= end

    def isIncluded(other: IntRange): Boolean = other.start <= start && other.end >= end

    def overlapsByStart(other: IntRange): Boolean = other.start <= start && other.end >= start && other.end <= end

    def overlapsByEnd(other: IntRange): Boolean = other.end >= end && other.start >= start && other.start <= end

    def union(range: IntRange): (IntRange, Option[IntRange]) = if (includes(range)) {
      (this, None)
    } else if (isIncluded(range)) {
      (range, None)
    } else if (overlapsByStart(range)) {
      (IntRange(range.start, end), None)
    } else if (overlapsByEnd(range)) {
      (IntRange(start, range.end), None)
    } else {
      (this, Some(range))
    }

    def length: Int = 1 + end - start

    def limit(limits: Option[(Int, Int)]): Option[IntRange] = {
      limits match
        case Some((min, max)) =>
          if (start < min) {
            if (end < min) {
              None
            } else if (end >= max) {
              Some(IntRange(min, max))
            } else {
              Some(IntRange(min, end))
            }
          } else if (end >= max) {
            if (start <= max) {
              Some(IntRange(start, max))
            } else {
              None
            }
          } else {
            Some(this)
          }
        case None => Some(this)
    }

  }

  final case class MultiRange(ranges: List[IntRange], limits: Option[(Int, Int)] = None) {
    def size: Int = ranges.map(_.length).sum

    def free: Option[Int] = limits.flatMap { case (min, max) =>
      ranges.map(_.toRange).foldLeft((min to max).toSet) { case (res, range) => res -- range }.headOption
    }

    def add(range: IntRange): MultiRange = {
      range.limit(limits) match
        case Some(value) => MultiRange(union(ranges, value), limits)
        case None        => this
    }

    def union(unions: List[IntRange], range: IntRange): List[IntRange] = unions match
      case Nil => List(range)
      case head :: tail =>
        head.union(range) match
          case (first, None)         => union(tail, first.limit(limits).get)
          case (first, Some(second)) => first :: union(tail, second)

    def remove(i: Int): MultiRange = ranges.find(r => r.start <= i && i <= r.end) match
      case Some(value) =>
        if (value.start == i) {
          MultiRange((ranges.toSet - value + IntRange(value.start + 1, value.end)).toList)
        } else if (value.end == i) {
          MultiRange((ranges.toSet - value + IntRange(value.start, value.end - 1)).toList)
        } else {
          MultiRange((ranges.toSet - value + IntRange(value.start, i - 1) + IntRange(i + 1, value.end)).toList)
        }
      case None => this

    def removeAll(r: Seq[Int]): MultiRange = r.foldLeft(this)((m, i) => m.remove(i))
  }

  object MultiRange {
    def empty(limits: Option[(Int, Int)]): MultiRange = MultiRange(List.empty, limits)
  }

  extension (c: Coordinates) {
    def withinInLine(manhattanDistance: Int, line: Int): Set[Coordinates] =
      (for {
        x1 <- 0 to manhattanDistance
        y1 <- 0 to (manhattanDistance - x1) if (c.y + y1) == line || (c.y - y1) == line
        c1 <-
          if (c.y + y1 == line) Set(Coordinates(c.x + x1, c.y + y1), Coordinates(c.x - x1, c.y + y1))
          else Set(Coordinates(c.x + x1, c.y - y1), Coordinates(c.x - x1, c.y - y1))
      } yield c1).toSet

    def rangeWithinInLine(manhattanDistance: Int, line: Int): IntRange =
      val maxX = manhattanDistance - Math.abs(c.y - line)
      IntRange(c.x - maxX, c.x + maxX)
  }
}
