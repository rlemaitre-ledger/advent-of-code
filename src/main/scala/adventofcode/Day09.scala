package adventofcode

import scala.annotation.tailrec

object Day09 extends AdventOfCodeBase[Int, Int]("day09.txt") {
  override def part1(lines: List[String]): Int = play(State.init(2), moves(lines)).tail.last.past.size
  override def part2(lines: List[String]): Int = play(State.init(10), moves(lines)).tail.last.past.size
  def moves(lines: List[String]): List[Move]   = lines.map(Move.parse)
  @tailrec
  def play(state: State, moves: List[Move]): State = {
    moves match
      case Nil => state
      case head :: next =>
        if (head.length == 0) {
          play(state, next)
        } else {
          val nextState = state.moveHead(head.direction)
          play(nextState, head.consumed :: next)
        }
  }
  final case class Move(direction: Direction, length: Int) {
    def consumed: Move = copy(length = length - 1)
  }
  object Move {
    def parse(string: String): Move = {
      val parts = string.split(' ')
      Move(Direction.parse(parts.head), parts.last.toInt)
    }
  }
  enum Direction {
    case Up, Down, Left, Right
  }
  object Direction {
    def parse(str: String): Direction = str match
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
  }
  final case class Position(x: Int, y: Int) {
    def adjacentTo(other: Position): Boolean =
      Math.abs(x - other.x) <= 1 && Math.abs(y - other.y) <= 1
    def move(direction: Direction): Position =
      direction match
        case Direction.Up    => Position(x, y + 1)
        case Direction.Down  => Position(x, y - 1)
        case Direction.Left  => Position(x - 1, y)
        case Direction.Right => Position(x + 1, y)
    def sameLine(other: Position): Boolean = x == other.x
    def sameRow(other: Position): Boolean  = y == other.y
    def directionTo(other: Position): Option[List[Direction]] =
      if (adjacentTo(other)) {
        None
      } else if (sameRow(other)) {
        if (x < other.x) {
          Some(List(Direction.Right))
        } else {
          Some(List(Direction.Left))
        }
      } else if (sameLine(other)) {
        if (y < other.y) {
          Some(List(Direction.Up))
        } else {
          Some(List(Direction.Down))
        }
      } else {
        if (x < other.x) {
          if (y < other.y) {
            Some(List(Direction.Up, Direction.Right))
          } else {
            Some(List(Direction.Down, Direction.Right))
          }
        } else {
          if (y < other.y) {
            Some(List(Direction.Up, Direction.Left))
          } else {
            Some(List(Direction.Down, Direction.Left))
          }
        }
      }
    def moveTowards(other: Position): Position =
      directionTo(other) match
        case Some(directions) => directions.foldLeft(this) { case (pos, dir) => pos.move(dir) }
        case None             => this
  }
  object Position {
    val start: Position = Position(0, 0)
  }
  final case class Path(current: Position, past: Set[Position]) {
    def move(direction: Direction): Path = {
      val next = current.move(direction)
      Path(next, past + next)
    }
    def moveTowards(path: Path): Path = {
      val next = current.moveTowards(path.current)
      Path(next, past + next)
    }
  }
  object Path {
    val start: Path = Path(Position.start, Set(Position.start))
  }
  final case class State(head: Path, tail: List[Path]) {
    def moveHead(direction: Direction): State = {
      val nextHead = head.move(direction)
      val nextTail = tail
        .foldLeft((List.empty[Path], nextHead)) { case ((acc, previous), current) =>
          val next = current.moveTowards(previous)
          (acc :+ next, next)
        }
        ._1
      copy(head = nextHead, tail = nextTail)
    }
  }
  object State {
    val start: State         = State(Path.start, List(Path.start))
    def init(nb: Int): State = State(Path.start, (0 until (nb - 1)).toList.map(_ => Path.start))
  }
}
