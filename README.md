# Advent of code

<p align="center">
	<a href="https://gitmoji.dev">
		<img src="https://img.shields.io/badge/gitmoji-%20😜%20😍-FFDD67.svg?style=flat-square"
			 alt="Gitmoji">
	</a>
    <img src="https://github.com/rlemaitre-ledger/advent-of-code-2022/actions/workflows/ci.yml/badge.svg">
    <a href="https://codecov.io/gh/rlemaitre-ledger/advent-of-code-2022">
        <img src="https://codecov.io/gh/rlemaitre-ledger/advent-of-code-2022/branch/main/graph/badge.svg?token=5XW9EJ5SBD"/>
    </a>
</p>

This repository contains my code for [Advent of Code](https://adventofcode.com/).

## Achievements

### 2022

[Problems](https://adventofcode.com/2022)

| Day                                                             | Title                                                           | Result |
|-----------------------------------------------------------------|-----------------------------------------------------------------|--------|
| [Day 1 ](src/main/scala/adventofcode/aoc2022/day01/Day01.scala) | [ Calorie Counting](https://adventofcode.com/2022/day/1)        | ⭐️⭐️   |
| [Day 2 ](src/main/scala/adventofcode/aoc2022/day02/Day02.scala) | [Rock Paper Scissors](https://adventofcode.com/2022/day/2)      | ⭐️⭐️   |
| [Day 3 ](src/main/scala/adventofcode/aoc2022/day03/Day03.scala) | [Rucksack Reorganization](https://adventofcode.com/2022/day/3)  | ⭐️⭐️   |
| [Day 4 ](src/main/scala/adventofcode/aoc2022/day04/Day04.scala) | [Camp Cleanup](https://adventofcode.com/2022/day/4)             | ⭐️⭐️   |
| [Day 5 ](src/main/scala/adventofcode/aoc2022/day05/Day05.scala) | [Supply Stacks](https://adventofcode.com/2022/day/5)            | ⭐️⭐️   |
| [Day 6 ](src/main/scala/adventofcode/aoc2022/day06/Day06.scala) | [Tuning Trouble](https://adventofcode.com/2022/day/6)           | ⭐️⭐️   |
| [Day 7 ](src/main/scala/adventofcode/aoc2022/day07/Day07.scala) | [No Space Left On Device](https://adventofcode.com/2022/day/7)  | ⭐️⭐️   |
| [Day 8 ](src/main/scala/adventofcode/aoc2022/day08/Day08.scala) | [Treetop Tree House](https://adventofcode.com/2022/day/8)       | ⭐️⭐️   |
| [Day 9 ](src/main/scala/adventofcode/aoc2022/day09/Day09.scala) | [Rope Bridge](https://adventofcode.com/2022/day/9)              | ⭐️⭐️   |
| [Day 10](src/main/scala/adventofcode/aoc2022/day10/Day10.scala) | [Cathode-Ray Tube](https://adventofcode.com/2022/day/10)        | ⭐️⭐️   |
| [Day 11](src/main/scala/adventofcode/aoc2022/day11/Day11.scala) | [Monkey in the Middle](https://adventofcode.com/2022/day/11)    | ⭐️⭐️   |
| [Day 12](src/main/scala/adventofcode/aoc2022/day12/Day12.scala) | [Hill Climbing Algorithm](https://adventofcode.com/2022/day/12) | ⭐️⭐️   |
| [Day 13](src/main/scala/adventofcode/aoc2022/day13/Day13.scala) | [Distress Signal](https://adventofcode.com/2022/day/13)         | ⭐️⭐️   |
| [Day 14](src/main/scala/adventofcode/aoc2022/day14/Day14.scala) | [Regolith Reservoir](https://adventofcode.com/2022/day/14)      | ⭐️⭐️   |
| [Day 15](src/main/scala/adventofcode/aoc2022/day15/Day15.scala) | [Beacon Exclusion Zone](https://adventofcode.com/2022/day/15)   | ⭐️⭐️   |
| [Day 16](src/main/scala/adventofcode/aoc2022/day16/Day16.scala) | [Proboscidea Volcanium](https://adventofcode.com/2022/day/16)   | ⭐️⭐️   |
| [Day 17](src/main/scala/adventofcode/aoc2022/day17/Day17.scala) | [Pyroclastic Flow](https://adventofcode.com/2022/day/17)        | ⭐️⭐️   |
| [Day 18](src/main/scala/adventofcode/aoc2022/day18/Day18.scala) | [Boiling Boulders](https://adventofcode.com/2022/day/18)        | ⭐️⭐️   |
| Day 19                                                          |                                                                 | ✩✩     |
| Day 20                                                          |                                                                 | ✩✩     |
| Day 21                                                          |                                                                 | ✩✩     |
| Day 22                                                          |                                                                 | ✩✩     |
| Day 23                                                          |                                                                 | ✩✩     |
| Day 24                                                          |                                                                 | ✩✩     |
| Day 25                                                          |                                                                 | ✩✩     |

## Tech Stack

**Language:** Plain Scala 3 (`v3.2.1`) with [Parser Combinators](https://github.com/scala/scala-parser-combinators) (`v2.1.1`)

**Tests:** [mUnit](https://scalameta.org/munit/)

## Running Tests

To run tests, run the following command

```bash
  sbt test
```

## Running with AoC input

To find answers to AoC problem, run the following commands

```bash
  sbt console
  > import adventofcode.*
  > import adventofcode.aocXXXX.dayXX.*
  > DayXX.instance.run
```

To run only Part 1

```bash
  sbt console
  > import adventofcode.*
  > import adventofcode.aocXXXX.dayXX.*
  > DayXX.instance.run(Mode.Part1)
```

To run only Part 2

```bash
  sbt console
  > import adventofcode.*
  > import adventofcode.aocXXXX.dayXX.*
  > DayXX.instance.run(Mode.Part2)
```

## License

[MIT](https://choosealicense.com/licenses/mit/)
