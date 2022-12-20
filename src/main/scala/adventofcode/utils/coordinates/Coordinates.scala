package adventofcode.utils.coordinates

import scala.annotation.targetName

final case class Coordinates(x: Int, y: Int) {
  def adjacentTo(other: Coordinates): Boolean =
    Math.abs(x - other.x) <= 1 && Math.abs(y - other.y) <= 1
  def move(direction: Direction): Coordinates = {
    direction match
      case Direction.Up => Coordinates(x, y + 1)
      case Direction.Down => Coordinates(x, y - 1)
      case Direction.Left => Coordinates(x - 1, y)
      case Direction.Right => Coordinates(x + 1, y)
  }
  def sameLine(other: Coordinates): Boolean = x == other.x
  def sameRow(other: Coordinates): Boolean = y == other.y
  def directionTo(other: Coordinates): List[Direction] = {
    if (adjacentTo(other))
      Nil
    else if (sameRow(other))
      if (x < other.x)
        List(Direction.Right)
      else
        List(Direction.Left)
    else if (sameLine(other))
      if (y < other.y)
        List(Direction.Up)
      else
        List(Direction.Down)
    else if (x < other.x)
      if (y < other.y)
        List(Direction.Up, Direction.Right)
      else
        List(Direction.Down, Direction.Right)
    else if (y < other.y)
      List(Direction.Up, Direction.Left)
    else
      List(Direction.Down, Direction.Left)
  }
  def neighbours: List[Coordinates] = Direction.values.map(move).toList
  def manhattanDistance(to: Coordinates): Int =
    Math.abs(to.x - x) + Math.abs(to.y - y)
  @targetName("plus")
  def +(other: Coordinates): Coordinates = Coordinates(this.x + other.x, this.y + other.y)
}
object Coordinates {
  val origin: Coordinates = Coordinates(0, 0)
}
