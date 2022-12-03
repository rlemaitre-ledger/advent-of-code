package adventofcode.day2

import munit.FunSuite

class Day2Test extends FunSuite {
  val input = """A Y
                |B X
                |C Z""".stripMargin.split('\n').toList

  test("parse part1") {
    assertEquals(
      Part1.rounds(input),
      List(
        (Move.Rock, Move.Paper, Result.Win),
        (Move.Paper, Move.Rock, Result.Loss),
        (Move.Scissors, Move.Scissors, Result.Draw)
      )
    )
  }

  test("compute score part 1") {
    assertEquals(computeScore(Part1.rounds(input)), 15)
  }
  test("parse part2") {
    assertEquals(
      Part2.rounds(input),
      List(
        (Move.Rock, Move.Rock, Result.Draw),
        (Move.Paper, Move.Rock, Result.Loss),
        (Move.Scissors, Move.Rock, Result.Win)
      )
    )
  }

  test("compute score part 2") {
    assertEquals(computeScore(Part2.rounds(input)), 12)
  }
}
