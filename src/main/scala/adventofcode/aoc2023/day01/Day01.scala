package adventofcode.aoc2023.day01
import Day01.*
import adventofcode.Problem
import adventofcode.inputLines

final case class Day01(input: List[String]) extends Problem[List[String], Int, Int](2023, 1, "Trebuchet?!"):
  override def part1: Int = input.map(calibrationValue(onlyDigits)).sum
  override def part2: Int = input.map(calibrationValue(digitsAndSpelledOut)).sum

object Day01:
  val instance: Day01 = Day01(inputLines("2023/day01.txt"))
  private type DigitExtractor = String => List[Char]

  def calibrationValue(extractor: DigitExtractor)(line: String): Int =
    s"${firstDigit(extractor)(line)}${lastDigit(extractor)(line)}".toInt

  val onlyDigits: DigitExtractor = _.filter(_.isDigit).toList

  private def firstDigit(extractDigit: DigitExtractor)(line: String): Char = extractDigit(line).head
  def lastDigit(extractDigit: DigitExtractor)(line: String): Char          = extractDigit(line).last

  val digitsAndSpelledOut: DigitExtractor = line =>
    line
      .foldLeft(""): (acc, c) =>
        (acc + c)
          .replaceAll("one", "1e")
          .replaceAll("two", "2o")
          .replaceAll("three", "3e")
          .replaceAll("four", "4r")
          .replaceAll("five", "5f")
          .replaceAll("six", "6x")
          .replaceAll("seven", "7n")
          .replaceAll("eight", "8t")
          .replaceAll("nine", "9e")
      .filter(_.isDigit)
      .toList
