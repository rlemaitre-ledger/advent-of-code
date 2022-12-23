package adventofcode.aoc2022.day22

import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Direction
import adventofcode.utils.coordinates.Direction.*
import scala.annotation.tailrec

final case class Game(
    remainingMoves: List[Move],
    board: Board,
    direction: Direction,
    position: Coordinates,
    nbSteps: Int = 0
) {
  val isOver: Boolean = remainingMoves.isEmpty
  val password: Int = 1000 * position.y + 4 * position.x + (direction match
    case Direction.Up    => 3
    case Direction.Down  => 1
    case Direction.Left  => 2
    case Direction.Right => 0
  )
  @tailrec
  private def goForward(current: Coordinates, n: Int): Coordinates =
    if n == 0 then current
    else {
      val tile = board.tiles(current)
      val next = direction match
        case Direction.Up    => tile.up
        case Direction.Down  => tile.down
        case Direction.Left  => tile.left
        case Direction.Right => tile.right
      if board.tiles(next).wall then current else goForward(next, n - 1)
    }
  def next: Game = remainingMoves.head match
    case Move.Forward(n) =>
      copy(
        remainingMoves = remainingMoves.tail,
        nbSteps = nbSteps + 1,
        position = goForward(position, n)
      )
    case Move.TurnLeft =>
      copy(
        remainingMoves = remainingMoves.tail,
        nbSteps = nbSteps + 1,
        direction = direction match
          case Up    => Left
          case Down  => Right
          case Left  => Down
          case Right => Up
      )
    case Move.TurnRight =>
      copy(
        remainingMoves = remainingMoves.tail,
        nbSteps = nbSteps + 1,
        direction = direction match
          case Up    => Right
          case Down  => Left
          case Left  => Up
          case Right => Down
      )
}
