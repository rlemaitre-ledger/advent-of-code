package adventofcode.day2

import adventofcode.AdventOfCodeBase
import scala.io.Source

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

def computeScore(rounds: List[(Move, Move, Result)]): Int = {
  rounds.map { case (_, move, result) => move.score + result.score }.sum
}

object Part1 extends AdventOfCodeBase("day2.txt") {
  def parseLine(line: String): (Move, Move) = {
    val arr = line.split(' ')
    assert(arr.length == 2, s"line [$line] is invalid")
    (Move.fromOpponent(arr.head), Move.fromMe(arr.last))
  }
  def rounds(lines: List[String]): List[(Move, Move, Result)] =
    lines
      .map(parseLine)
      .map { case (opponent, me) => (opponent, me, Result.from(opponent, me)) }

  def main(args: Array[String]): Unit = {
    val myScore = computeScore(rounds(input))
    println(s"my score is $myScore")
  }
}

object Part2 extends AdventOfCodeBase("day2.txt") {
  def parseLine(line: String): (Move, Result) = {
    val arr = line.split(' ')
    assert(arr.length == 2, s"line [$line] is invalid")
    (Move.fromOpponent(arr.head), Result.fromInput(arr.last))
  }

  def rounds(lines: List[String]): List[(Move, Move, Result)] =
    lines
      .map(parseLine)
      .map { case (opponent, result) => (opponent, Move.from(opponent, result), result) }

  def main(args: Array[String]): Unit = {
    val myScore = computeScore(rounds(input))
    println(s"my score is $myScore")
  }
}
