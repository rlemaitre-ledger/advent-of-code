import scala.annotation.nowarn
import scala.io.Source

package object adventofcode:
  def inputLines(path: String): List[String] = Source
    .fromResource(path)
    .getLines()
    .toList

  def time[A](f: => A): A = {
    val s   = System.nanoTime
    val ret = f
    println("time: " + (System.nanoTime - s) / 1e6 + "ms")
    ret
  }
  enum Mode:
    case Part1, Part2
  enum Direction:
    case Up, Down, Left, Right
  object Direction:
    def parse(str: String): Direction = str match
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
  final case class Coordinates(x: Int, y: Int):
    def adjacentTo(other: Coordinates): Boolean =
      Math.abs(x - other.x) <= 1 && Math.abs(y - other.y) <= 1
    def move(direction: Direction): Coordinates =
      direction match
        case Direction.Up    => Coordinates(x, y + 1)
        case Direction.Down  => Coordinates(x, y - 1)
        case Direction.Left  => Coordinates(x - 1, y)
        case Direction.Right => Coordinates(x + 1, y)
    def sameLine(other: Coordinates): Boolean = x == other.x
    def sameRow(other: Coordinates): Boolean  = y == other.y
    def directionTo(other: Coordinates): List[Direction] =
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
    def neighbours: List[Coordinates] = Direction.values.map(move).toList
    def manhattanDistance(to: Coordinates): Int =
      Math.abs(to.x - x) + Math.abs(to.y - y)
    def plus(other: Coordinates): Coordinates = Coordinates(this.x + other.x, this.y + other.y)
  object Coordinates:
    val origin: Coordinates = Coordinates(0, 0)

  def bfs[A](start: A, graph: A => Seq[A]): Map[A, Int] = {
    val todo = collection.mutable.Queue(start)
    val cost = collection.mutable.Map(start -> 1)

    while todo.nonEmpty do
      val current = todo.dequeue()
      graph(current).filterNot(cost.contains).foreach { next =>
        todo.enqueue(next): @nowarn
        cost(next) = cost(current) + 1
      }

    cost.toMap
  }
