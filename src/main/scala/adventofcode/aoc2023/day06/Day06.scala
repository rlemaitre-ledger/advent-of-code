package adventofcode.aoc2023.day06
import adventofcode.Problem
import adventofcode.aoc2023.day06.Day06.optimized
import adventofcode.inputLines
import java.math.MathContext
import scala.math.BigDecimal.RoundingMode

final case class Day06(input: List[String]) extends Problem[List[String], Long, Long](2023, 6, "Wait For It"):
  override def part1: Long = Races.parse(input).map(race => if optimized then race.winCount else race.run).product
  override def part2: Long = Races.parseSingle(input).map(race => if optimized then race.winCount else race.run).product

object Day06:
  val optimized: Boolean = true
  val instance: Day06    = Day06(inputLines("2023/day06.txt"))

final case class Boat(speed: Long):
  private val acceleration: Long         = 1
  def buttonPushed(duration: Long): Boat = copy(speed = speed + duration * acceleration)

object Boat:
  val initial: Boat = Boat(0)

final case class Race(duration: Long, distance: Long):
  def simulate(boat: Boat, holdButtonDuration: Long): Result =
    val length = (duration - holdButtonDuration) * boat.buttonPushed(holdButtonDuration).speed
    Result(length, length > distance)

  def run: Long =
    (0L to duration)
      .map: d =>
        simulate(Boat.initial, d)
      .count(_.won)

  extension (a: BigDecimal) //
    def sqrt: BigDecimal =
      BigDecimal(Math.sqrt(a.doubleValue), MathContext.DECIMAL64)
  def winCount: Long =
    val a         = BigDecimal(duration)
    val b         = BigDecimal(distance)
    val solution1 = BigDecimal(0.5) * (a - (a * a - 4 * b).sqrt)
    val solution2 = BigDecimal(0.5) * (a + (a * a - 4 * b).sqrt)
    val to        = solution2.setScale(0, RoundingMode.DOWN).longValue
    val from      = solution1.setScale(0, RoundingMode.DOWN).longValue
    val start     = if simulate(Boat.initial, from).won then from else from + 1
    val end       = if simulate(Boat.initial, to).won then to else to - 1
    end - start + 1

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
