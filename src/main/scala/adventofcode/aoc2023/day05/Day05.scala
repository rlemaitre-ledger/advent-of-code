package adventofcode.aoc2023.day05

import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.range.*

final case class Day05(input: List[String])
    extends Problem[List[String], Long, Long](2023, 5, "If You Give A Seed A Fertilizer"):
  override def part1: Long = Almanac.parse1(input).lowestLocation
  override def part2: Long = Almanac.parse2(input).lowestLocation

object Day05:
  val instance: Day05 = Day05(inputLines("2023/day05.txt"))

final case class Almanac(seeds: MultiRange[Long], transformations: List[Transform]):
  private def getLocation(seed: Long): Long = transformations.foldLeft(seed)((acc, t) => t.transform(acc))

  def lowestLocation: Long = transformations.foldLeft(seeds)((m, t) => t.transformMultiRange(m)).head.from

object Almanac:
  def parse1(lines: List[String]): Almanac =
    val seeds = MultiRange(
      lines.head.replace("seeds: ", "").split(' ').map(_.toLong).toList.map(n => Range.single(n))
    )
    Almanac(seeds, parse(lines))
  def parse2(lines: List[String]): Almanac =
    val numbers = lines.head.replace("seeds: ", "").split(' ')
    val seeds: MultiRange[Long] = MultiRange(
      numbers
        .grouped(2)
        .map: pair =>
          val start  = pair.head.toLong
          val length = pair.last.toLong
          Range(start, start + length - 1)
        .toList
    )
    Almanac(seeds, parse(lines))
  def parse(lines: List[String]): List[Transform] =
    val (seedToSoilDef, rest)             = lines.drop(3).span(_.nonEmpty)
    val seedToSoil                        = Transform.parse(seedToSoilDef)
    val (soilToFertilizerDef, rest2)      = rest.drop(2).span(_.nonEmpty)
    val soilToFertilizer                  = Transform.parse(soilToFertilizerDef)
    val (fertilizerToWaterDef, rest3)     = rest2.drop(2).span(_.nonEmpty)
    val fertilizerToWater                 = Transform.parse(fertilizerToWaterDef)
    val (waterToLightDef, rest4)          = rest3.drop(2).span(_.nonEmpty)
    val waterToLight                      = Transform.parse(waterToLightDef)
    val (lightToTemperatureDef, rest5)    = rest4.drop(2).span(_.nonEmpty)
    val lightToTemperature                = Transform.parse(lightToTemperatureDef)
    val (temperatureToHumidityDef, rest6) = rest5.drop(2).span(_.nonEmpty)
    val temperatureToHumidity             = Transform.parse(temperatureToHumidityDef)
    val humidityToLocationDef             = rest6.drop(2)
    val humidityToLocation                = Transform.parse(humidityToLocationDef)
    List(
      seedToSoil,
      soilToFertilizer,
      fertilizerToWater,
      waterToLight,
      lightToTemperature,
      temperatureToHumidity,
      humidityToLocation
    )

final case class Conversion(destinationStart: Long, sourceStart: Long, length: Long):
  def validFor(input: Long): Boolean = input >= sourceStart && input < sourceStart + length
  def convert(input: Long): Long     = destinationStart + input - sourceStart

  val sourceRange: Range[Long]      = Range(sourceStart, sourceStart + length - 1)
  val destinationRange: Range[Long] = Range(destinationStart, destinationStart + length - 1)
final case class Transform(conversions: List[Conversion]):
  def transform(input: Long): Long = conversions.find(_.validFor(input)) match
    case Some(value) => value.convert(input)
    case None        => input

  private def transformRange(range: Range[Long]): MultiRange[Long] =
    val intervals: List[(Range[Long], Range[Long])] = conversions.flatMap: c =>
      Option.when(range overlaps c.sourceRange):
        val intersection = range intersect c.sourceRange
        val converted    = Range(c.convert(intersection.from), c.convert(intersection.to))
        (intersection, converted)
    val nonConverted = intervals.foldLeft(MultiRange.of(range))((acc, pair) => acc - pair._1)
    MultiRange(intervals.map(_._2)) + nonConverted

  def transformMultiRange(multiRange: MultiRange[Long]): MultiRange[Long] = {
    MultiRange.concat(multiRange.map(r => transformRange(r)))
  }

object Transform:
  def parse(lines: List[String]): Transform =
    val conversions: List[Conversion] =
      lines
        .map: line =>
          val parts = line.split(' ')
          Conversion(parts(0).toLong, parts(1).toLong, parts(2).toLong)
    Transform(conversions)
