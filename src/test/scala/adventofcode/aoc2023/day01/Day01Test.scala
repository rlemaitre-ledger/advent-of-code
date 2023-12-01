package adventofcode.aoc2023.day01

import Day01.*
import adventofcode.AoCTest

class Day01Test extends AoCTest:
  override val lines: String =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet
      |""".stripMargin
  val input2: List[String] = """two1nine
                     |eightwothree
                     |abcone2threexyz
                     |xtwone3four
                     |4nineeightseven2
                     |zoneight234
                     |7pqrstsixteen""".stripMargin.split('\n').toList

  test("part 1") {
    assertEquals(Day01(input).part1, 142)
  }
  test("Complete part 1") {
    assertEquals(Day01.instance.part1, 54697)
  }
  test("part 2") {
    assertEquals(Day01(input2).part2, 281)
  }

  test("onlyDigits") {
    assertEquals(
      input.map(Day01.onlyDigits),
      List(List('1', '2'), List('3', '8'), List('1', '2', '3', '4', '5'), List('7'))
    )
  }

  test("digitsAndSpelledOut") {
    assertEquals(
      input2.map(Day01.digitsAndSpelledOut).map(_.mkString),
      List("219", "823", "123", "2134", "49872", "18234", "76")
    )
    assertEquals(digitsAndSpelledOut("6mgcffzzspll15djsoneseven").mkString, "61517")
    assertEquals(
      digitsAndSpelledOut("gsskzxkgrbzx5pggzmsfhtwotrsvmttzdc8three1").mkString,
      "52831"
    )
  }

  test("last digit") {
    assertEquals(lastDigit(onlyDigits)("6mgcffzzspll15djsoneseven"), '5')
    assertEquals(lastDigit(digitsAndSpelledOut)("6mgcffzzspll15djsoneseven"), '7')
  }

  test("calibration value") {
    assertEquals(calibrationValue(digitsAndSpelledOut)("eighthree"), 83)
    assertEquals(calibrationValue(digitsAndSpelledOut)("sevenine"), 79)
  }