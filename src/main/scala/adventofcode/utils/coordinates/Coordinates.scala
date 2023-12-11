package adventofcode.utils.coordinates

import scala.annotation.targetName

final case class Coordinates(x: Int, y: Int):
  def adjacentTo(other: Coordinates): Boolean =
    Math.abs(x - other.x) <= 1 && Math.abs(y - other.y) <= 1
  def up: Coordinates    = move(Direction.Up)
  def down: Coordinates  = move(Direction.Down)
  def left: Coordinates  = move(Direction.Left)
  def right: Coordinates = move(Direction.Right)
  def move(direction: Direction): Coordinates =
    direction match
      case Direction.Up    => Coordinates(x, y + 1)
      case Direction.Down  => Coordinates(x, y - 1)
      case Direction.Left  => Coordinates(x - 1, y)
      case Direction.Right => Coordinates(x + 1, y)
  def sameColumn(other: Coordinates): Boolean = x == other.x
  def sameLine(other: Coordinates): Boolean   = y == other.y
  def directionTo(other: Coordinates): List[Direction] =
    if adjacentTo(other) then Nil
    else if sameLine(other) then if x < other.x then List(Direction.Right) else List(Direction.Left)
    else if sameColumn(other) then if y < other.y then List(Direction.Up) else List(Direction.Down)
    else if x < other.x then
      if y < other.y then List(Direction.Up, Direction.Right) else List(Direction.Down, Direction.Right)
    else if y < other.y then List(Direction.Up, Direction.Left)
    else List(Direction.Down, Direction.Left)
  def neighbours: List[Coordinates]           = Direction.values.map(move).toList
  def manhattanDistance(to: Coordinates): Int = Math.abs(to.x - x) + Math.abs(to.y - y)
  @targetName("plus")
  def +(other: Coordinates): Coordinates = Coordinates(this.x + other.x, this.y + other.y)

object Coordinates:
  val origin: Coordinates    = Coordinates(0, 0)
  val west: Coordinates      = Coordinates(-1, 0)
  val northWest: Coordinates = Coordinates(-1, 1)
  val north: Coordinates     = Coordinates(0, 1)
  val northEast: Coordinates = Coordinates(1, 1)
  val east: Coordinates      = Coordinates(1, 0)
  val southEast: Coordinates = Coordinates(1, -1)
  val south: Coordinates     = Coordinates(0, -1)
  val southWest: Coordinates = Coordinates(-1, -1)
