package adventofcode.aoc2023.day06
import adventofcode.Problem
import adventofcode.inputLines

final case class Day06(input: List[String]) extends Problem[List[String], Long, Long](2023, 6, "Wait For It"):
  override def part1: Long = {
    Races.parse(input).map(_.run).product
  }
  override def part2: Long =
    Races.parseSingle(input).map(_.run).product

object Day06:
  val instance: Day06 = Day06(inputLines("2023/day06.txt"))

final case class Boat(speed: Long):
  val acceleration: Long                 = 1
  def buttonPushed(duration: Long): Boat = copy(speed = speed + duration * acceleration)

object Boat:
  val initial: Boat = Boat(0)

final case class Race(duration: Long, distance: Long):
  def simulate(boat: Boat, holdButtonDuration: Long): Result =
    val length = (duration - holdButtonDuration) * boat.buttonPushed(holdButtonDuration).speed
//    println(s"Hold during $holdButtonDuration, boat travelled $length")
    Result(length, length > distance)

  def run: Long =
    (0L to duration)
      .map: d =>
        simulate(Boat.initial, d)
      .count(_.won)

case class Result(distance: Long, won: Boolean)
object Races:
  def parse(lines: List[String]): List[Race] =
    val times =
      lines.head.replace("Time:", "").replaceAll(raw" +", " ").trim.split(' ').filter(_.nonEmpty).map(_.toLong)
    val durations =
      lines.last.replace("Distance:", "").replaceAll(raw" +", " ").trim.split(' ').filter(_.nonEmpty).map(_.toLong)
    (times zip durations).map(Race.apply).toList

  def parseSingle(lines: List[String]): List[Race] =
    val times =
      lines.head.replace("Time:", "").replaceAll(raw" +", "").trim.split(' ').filter(_.nonEmpty).map(_.toLong)
    val durations =
      lines.last.replace("Distance:", "").replaceAll(raw" +", "").trim.split(' ').filter(_.nonEmpty).map(_.toLong)
    (times zip durations).map(Race.apply).toList
