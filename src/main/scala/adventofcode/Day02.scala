package adventofcode

import Day02.*
case class Day02(input: List[String]) extends AdventOfCodeBase[List[String], Int, Int]():
  def part1Rounds(lines: List[String]): List[Round] =
    lines
      .map(line => {
        val arr = line.split(' ')
        Round.fromMoves(Move.fromOpponent(arr.head), Move.fromMe(arr.last))
      })
  def part2Rounds(lines: List[String]): List[Round] =
    lines
      .map(line => {
        val arr = line.split(' ')
        Round.fromOpponentAndResult(Move.fromOpponent(arr.head), Result.fromInput(arr.last))
      })
  def computeScore(rounds: List[Round]): Int = rounds.map(_.score).sum
  override def part1: Int                    = computeScore(part1Rounds(input))
  override def part2: Int                    = computeScore(part2Rounds(input))
object Day02 {
  val instance: Day02 = Day02(inputLines("day02.txt"))
  enum Move(val score: Int) {
    case Rock     extends Move(1)
    case Paper    extends Move(2)
    case Scissors extends Move(3)
  }

  object Move {
    def fromOpponent(str: String): Move = str match
      case "A" => Move.Rock
      case "B" => Move.Paper
      case "C" => Move.Scissors
      case _   => throw new IllegalArgumentException()

    def fromMe(str: String): Move = str match
      case "X" => Move.Rock
      case "Y" => Move.Paper
      case "Z" => Move.Scissors
      case _   => throw new IllegalArgumentException()

    def from(opponentMove: Move, result: Result): Move = (opponentMove, result) match
      case (Move.Rock, Result.Win)      => Move.Paper
      case (Move.Rock, Result.Loss)     => Move.Scissors
      case (Move.Rock, Result.Draw)     => Move.Rock
      case (Move.Paper, Result.Win)     => Move.Scissors
      case (Move.Paper, Result.Loss)    => Move.Rock
      case (Move.Paper, Result.Draw)    => Move.Paper
      case (Move.Scissors, Result.Win)  => Move.Rock
      case (Move.Scissors, Result.Loss) => Move.Paper
      case (Move.Scissors, Result.Draw) => Move.Scissors
  }

  enum Result(val score: Int) {
    case Win  extends Result(6)
    case Draw extends Result(3)
    case Loss extends Result(0)
  }

  object Result {
    def fromInput(str: String): Result = str match
      case "X" => Result.Loss
      case "Y" => Result.Draw
      case "Z" => Result.Win
      case _   => throw new IllegalArgumentException()

    def from(opponent: Move, me: Move): Result = (opponent, me) match
      case (Move.Rock, Move.Rock)         => Result.Draw
      case (Move.Rock, Move.Paper)        => Result.Win
      case (Move.Rock, Move.Scissors)     => Result.Loss
      case (Move.Paper, Move.Rock)        => Result.Loss
      case (Move.Paper, Move.Paper)       => Result.Draw
      case (Move.Paper, Move.Scissors)    => Result.Win
      case (Move.Scissors, Move.Rock)     => Result.Win
      case (Move.Scissors, Move.Paper)    => Result.Loss
      case (Move.Scissors, Move.Scissors) => Result.Draw
  }

  case class Round(opponent: Move, me: Move, result: Result) {
    val score: Int = me.score + result.score
  }

  object Round {
    def fromMoves(opponent: Move, me: Move): Round =
      Round(opponent, me, Result.from(opponent, me))

    def fromOpponentAndResult(opponent: Move, result: Result): Round =
      Round(opponent, Move.from(opponent, result), result)
  }
}
