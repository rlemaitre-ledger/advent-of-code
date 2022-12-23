package adventofcode.aoc2022.day22

import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.coordinates.Direction

final case class Day22(input: Game) extends Problem[Game, Int, Int](2022, 22, "Monkey Map") {
  override def part1: Int = {
    val game = Iterator.iterate(input)(_.next).dropWhile(!_.isOver).next()
    game.password
  }
  override def part2: Int = ???
}
object Day22 {
  val instance: Day22 = Day22(parse(inputLines("2022/day22.txt")))
  def parse(lines: List[String]): Game =
    val board = Board.parse(lines.takeWhile(_.nonEmpty))
    val moves = Move.parse(lines.last)
    Game(moves, board, Direction.Right, board.start)
}
