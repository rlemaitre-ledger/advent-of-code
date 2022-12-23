package adventofcode.aoc2021.day04
import adventofcode.Problem
import adventofcode.inputLines
import scala.annotation.tailrec

final case class Day04(input: List[String]) extends Problem[List[String], Int, Int](2021, 4, "Giant Squid") {
  private val draws: List[Int]    = input.head.split(',').map(_.toInt).toList
  private val boards: List[Board] = Board.parseLines(input.tail)
  override def part1: Int         = findFirstScore(draws, boards)

  override def part2: Int = findLastScore(draws, boards)
  @tailrec
  private def findFirstScore(remainingDraws: List[Int], playedBoards: List[Board]): Int =
    playedBoards.find(_.wins) match
      case Some(value: Board) => value.score
      case None               => findFirstScore(remainingDraws.tail, playedBoards.map(_.draw(remainingDraws.head)))

  @tailrec
  private def findLastScore(remainingDraws: List[Int], playedBoards: List[Board]): Int = {
    val notWinning = playedBoards.filterNot(_.wins)
    if notWinning.length == 1 then findFirstScore(remainingDraws, notWinning)
    else findLastScore(remainingDraws.tail, playedBoards.map(_.draw(remainingDraws.head)))
  }
}
object Day04 {
  val instance: Day04 = Day04(inputLines("2021/day04.txt"))
}
