package adventofcode.aoc2022.day17

import Day17.*
import State.*
import adventofcode.Coordinates
import scala.annotation.tailrec

final case class State(moves: String, grid: Grid, rockIndex: Int, moveIndex: Int, height: Int) {
  def step: State = {
    val initialRock               = Rock.all(rockIndex % Rock.all.length).move(Coordinates(3, height + 4))
    val (nextRock, nextMoveIndex) = fall(initialRock, moveIndex)
    val nextHeight                = height.max(nextRock.points.map(_.y).max)
    State(moves, grid.add(nextRock), rockIndex + 1, nextMoveIndex, nextHeight)
  }
  @tailrec
  def fall(rock: Rock, moveIndex: Int): (Rock, Int) = {
    val move   = moves(moveIndex % moves.length)
    val first  = rock.move(if move == '>' then rightMove else leftMove)
    val second = if first.canMove(grid) then first else rock
    val third  = second.move(downMove)
    if third.canMove(grid) then fall(third, moveIndex + 1) else (second, moveIndex + 1)
  }
}
object State {
  val leftMove: Coordinates  = Coordinates(-1, 0)
  val rightMove: Coordinates = Coordinates(1, 0)
  val downMove: Coordinates  = Coordinates(0, -1)
}
