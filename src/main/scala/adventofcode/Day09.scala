package adventofcode

import scala.annotation.tailrec

object Day09 extends AdventOfCodeBase[Int, Int]("day09.txt"):
  override def part1(lines: List[String]): Int = play(State.init(2), moves(lines)).tail.last.past.size
  override def part2(lines: List[String]): Int = play(State.init(10), moves(lines)).tail.last.past.size
  def moves(lines: List[String]): List[Move]   = lines.map(Move.parse)
  @tailrec
  def play(state: State, moves: List[Move]): State =
    moves match
      case Nil => state
      case head :: next =>
        if (head.length == 0) play(state, next)
        else
          val nextState = state.moveHead(head.direction)
          play(nextState, head.consumed :: next)
  final case class Move(direction: Direction, length: Int):
    def consumed: Move = copy(length = length - 1)
  object Move:
    def parse(string: String): Move =
      val parts = string.split(' ')
      Move(Direction.parse(parts.head), parts.last.toInt)
  extension (c: Coordinates)
    def moveTowards(other: Coordinates): Coordinates =
      c.directionTo(other) match
        case Nil        => c
        case directions => directions.foldLeft(c) { case (pos, dir) => pos.move(dir) }
  final case class Path(current: Coordinates, past: Set[Coordinates]):
    def move(direction: Direction): Path =
      val next = current.move(direction)
      Path(next, past + next)
    def moveTowards(path: Path): Path =
      val next = current.moveTowards(path.current)
      Path(next, past + next)

  object Path:
    val start: Path = Path(Coordinates.origin, Set(Coordinates.origin))
  final case class State(head: Path, tail: List[Path]):
    def moveHead(direction: Direction): State =
      val nextHead = head.move(direction)
      val nextTail = tail
        .foldLeft((List.empty[Path], nextHead)) { case ((acc, previous), current) =>
          val next = current.moveTowards(previous)
          (acc :+ next, next)
        }
        ._1
      copy(head = nextHead, tail = nextTail)
  object State:
    val start: State         = State(Path.start, List(Path.start))
    def init(nb: Int): State = State(Path.start, (0 until (nb - 1)).toList.map(_ => Path.start))
