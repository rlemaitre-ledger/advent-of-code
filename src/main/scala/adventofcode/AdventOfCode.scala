package adventofcode

import adventofcode.AdventOfCode.*
import cats.syntax.all.*
import com.monovore.decline.CommandApp
import com.monovore.decline.Opts
import java.time.LocalDate
// $COVERAGE-OFF$
object AdventOfCode
    extends CommandApp(
      name = "aoc",
      header = "Advent of Code launcher",
      helpFlag = true,
      version = "1.0.0",
      main = {
        val year: Opts[Int] = Opts
          .option[Int]("year", "Year of advent of code", "y")
          .withDefault(LocalDate.now().getYear)
        val day: Opts[Int] = Opts
          .option[Int]("day", "Day of advent of code", "d")
          .withDefault(LocalDate.now().getDayOfMonth)
        (year, day).mapN: (y, d) =>
          problems.get(y).foreach(_.get(d).foreach(_.run))
      }
    ):
  private val problems: Map[Int, Map[Int, Problem[_, _, _]]] =
    Map(
      2021 -> Map(
        1 -> aoc2021.day01.Day01.instance,
        2 -> aoc2021.day02.Day02.instance,
        3 -> aoc2021.day03.Day03.instance,
        4 -> aoc2021.day04.Day04.instance
      ),
      2022 -> Map(
        1  -> aoc2022.day01.Day01.instance,
        2  -> aoc2022.day02.Day02.instance,
        3  -> aoc2022.day03.Day03.instance,
        4  -> aoc2022.day04.Day04.instance,
        5  -> aoc2022.day05.Day05.instance,
        6  -> aoc2022.day06.Day06.instance,
        7  -> aoc2022.day07.Day07.instance,
        8  -> aoc2022.day08.Day08.instance,
        9  -> aoc2022.day09.Day09.instance,
        10 -> aoc2022.day10.Day10.instance,
        11 -> aoc2022.day11.Day11.instance,
        12 -> aoc2022.day12.Day12.instance,
        13 -> aoc2022.day13.Day13.instance,
        14 -> aoc2022.day14.Day14.instance,
        15 -> aoc2022.day15.Day15.instance,
        16 -> aoc2022.day16.Day16.instance,
        17 -> aoc2022.day17.Day17.instance,
        18 -> aoc2022.day18.Day18.instance,
        19 -> aoc2022.day19.Day19.instance,
        19 -> aoc2022.day19.Day19.instance,
        20 -> aoc2022.day20.Day20.instance,
        21 -> aoc2022.day21.Day21.instance,
        22 -> aoc2022.day22.Day22.instance,
        23 -> aoc2022.day23.Day23.instance
      ),
      2023 -> Map(
        1 -> aoc2023.day01.Day01.instance,
        2 -> aoc2023.day02.Day02.instance,
        3 -> aoc2023.day03.Day03.instance,
        4 -> aoc2023.day04.Day04.instance
//         5 -> aoc2023.day05.Day05.instance,
//         6 -> aoc2023.day06.Day06.instance,
//         7 -> aoc2023.day07.Day07.instance,
//         8 -> aoc2023.day08.Day08.instance,
//         9 -> aoc2023.day09.Day09.instance,
//        10 -> aoc2023.day10.Day10.instance,
//        11 -> aoc2023.day11.Day11.instance,
//        12 -> aoc2023.day12.Day12.instance,
//        13 -> aoc2023.day13.Day13.instance,
//        14 -> aoc2023.day14.Day14.instance,
//        15 -> aoc2023.day15.Day15.instance,
//        16 -> aoc2023.day16.Day16.instance,
//        17 -> aoc2023.day17.Day17.instance,
//        18 -> aoc2023.day18.Day18.instance,
//        19 -> aoc2023.day19.Day19.instance,
//        19 -> aoc2023.day19.Day19.instance,
//        20 -> aoc2023.day20.Day20.instance,
//        21 -> aoc2023.day21.Day21.instance,
//        22 -> aoc2023.day22.Day22.instance,
//        23 -> aoc2023.day23.Day23.instance,
//        24 -> aoc2023.day24.Day24.instance,
//        25 -> aoc2023.day25.Day25.instance,
      )
    )
// $COVERAGE-ON$
