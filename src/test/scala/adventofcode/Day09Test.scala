package adventofcode

import adventofcode.Day09.*
import org.scalacheck.Prop.*
import scala.annotation.nowarn

class Day09Test extends AoCTest {
  override val lines: String = """R 4
                                 |U 4
                                 |L 3
                                 |D 1
                                 |R 4
                                 |D 1
                                 |L 5
                                 |R 2""".stripMargin
  test("parse move") {
    assertEquals(
      Day09.moves(input),
      List(
        Move(Direction.Right, 4),
        Move(Direction.Up, 4),
        Move(Direction.Left, 3),
        Move(Direction.Down, 1),
        Move(Direction.Right, 4),
        Move(Direction.Down, 1),
        Move(Direction.Left, 5),
        Move(Direction.Right, 2)
      )
    )
  }

  property("positions is always adjacent to itself") {
    forAll { (x: Int, y: Int) =>
      Position(x, y).adjacentTo(Position(x, y))
    }
  }
  property("adjacent positions") {
    forAll { (x: Int, y: Int) =>
      Position(x, y).adjacentTo(Position(x + 1, y - 1)): @nowarn
      Position(x, y).adjacentTo(Position(x + 1, y)): @nowarn
      Position(x, y).adjacentTo(Position(x + 1, y + 1)): @nowarn
      Position(x, y).adjacentTo(Position(x - 1, y - 1)): @nowarn
      Position(x, y).adjacentTo(Position(x - 1, y)): @nowarn
      Position(x, y).adjacentTo(Position(x - 1, y + 1)): @nowarn
      Position(x, y).adjacentTo(Position(x, y - 1)): @nowarn
      Position(x, y).adjacentTo(Position(x, y + 1)): @nowarn

      !Position(x, y).adjacentTo(Position(x + 2, y - 2)): @nowarn
      !Position(x, y).adjacentTo(Position(x + 2, y)): @nowarn
      !Position(x, y).adjacentTo(Position(x + 2, y + 2)): @nowarn
      !Position(x, y).adjacentTo(Position(x - 2, y - 2)): @nowarn
      !Position(x, y).adjacentTo(Position(x - 2, y)): @nowarn
      !Position(x, y).adjacentTo(Position(x - 2, y + 2)): @nowarn
      !Position(x, y).adjacentTo(Position(x, y - 2)): @nowarn
      !Position(x, y).adjacentTo(Position(x, y + 2)): @nowarn
    }
  }
  property("adjacency is commutative") {
    forAll { (x1: Int, y1: Int, x2: Int, y2: Int) =>
      Position(x1, y1).adjacentTo(Position(x2, y2)) == Position(x2, y2).adjacentTo(Position(x1, y1))
    }
  }
  test("state move head") {
    val state  = State.start
    val state1 = state.moveHead(Direction.Right)
    assertEquals(state1.head.current, Position(1, 0))
    assertEquals(state1.tail.current, Position(0, 0))
    assertEquals(state1.head.past, Set(Position.start, Position(1, 0)))
    assertEquals(state1.tail.past, Set(Position.start))
    val state2 = state1.moveHead(Direction.Right)
    assertEquals(state2.head.current, Position(2, 0))
    assertEquals(state2.tail.current, Position(1, 0))
    assertEquals(state2.head.past, Set(Position.start, Position(1, 0), Position(2, 0)))
    assertEquals(state2.tail.past, Set(Position.start, Position(1, 0)))
    val state3 = state2.moveHead(Direction.Right)
    assertEquals(state3.head.current, Position(3, 0))
    assertEquals(state3.tail.current, Position(2, 0))
    assertEquals(state3.head.past, Set(Position.start, Position(1, 0), Position(2, 0), Position(3, 0)))
    assertEquals(state3.tail.past, Set(Position.start, Position(1, 0), Position(2, 0)))
    val state4 = state3.moveHead(Direction.Right)
    assertEquals(state4.head.current, Position(4, 0))
    assertEquals(state4.tail.current, Position(3, 0))
    assertEquals(
      state4.head.past,
      Set(Position.start, Position(1, 0), Position(2, 0), Position(3, 0), Position(4, 0))
    )
    assertEquals(state4.tail.past, Set(Position.start, Position(1, 0), Position(2, 0), Position(3, 0)))
    val state5 = state4.moveHead(Direction.Up)
    assertEquals(state5.head.current, Position(4, 1))
    assertEquals(state5.tail.current, Position(3, 0))
    assertEquals(
      state5.head.past,
      Set(Position.start, Position(1, 0), Position(2, 0), Position(3, 0), Position(4, 0), Position(4, 1))
    )
    assertEquals(state5.tail.past, Set(Position.start, Position(1, 0), Position(2, 0), Position(3, 0)))
    val state6 = state5.moveHead(Direction.Up)
    assertEquals(state6.head.current, Position(4, 2))
    assertEquals(state6.tail.current, Position(4, 1))
    assertEquals(
      state6.head.past,
      Set(
        Position.start,
        Position(1, 0),
        Position(2, 0),
        Position(3, 0),
        Position(4, 0),
        Position(4, 1),
        Position(4, 2)
      )
    )
    assertEquals(
      state6.tail.past,
      Set(Position.start, Position(1, 0), Position(2, 0), Position(3, 0), Position(4, 1))
    )
  }

  test("Direction to") {
    val from = Position(3, 3)
    assertEquals(from.directionTo(Position(5, 5)), Some(List(Direction.Up, Direction.Right)))
    assertEquals(from.directionTo(Position(5, 4)), Some(List(Direction.Up, Direction.Right)))
    assertEquals(from.directionTo(Position(5, 3)), Some(List(Direction.Right)))
    assertEquals(from.directionTo(Position(5, 2)), Some(List(Direction.Down, Direction.Right)))
    assertEquals(from.directionTo(Position(5, 1)), Some(List(Direction.Down, Direction.Right)))
    assertEquals(from.directionTo(Position(4, 5)), Some(List(Direction.Up, Direction.Right)))
    assertEquals(from.directionTo(Position(4, 4)), None)
    assertEquals(from.directionTo(Position(4, 3)), None)
    assertEquals(from.directionTo(Position(4, 2)), None)
    assertEquals(from.directionTo(Position(4, 1)), Some(List(Direction.Down, Direction.Right)))
    assertEquals(from.directionTo(Position(3, 5)), Some(List(Direction.Up)))
    assertEquals(from.directionTo(Position(3, 4)), None)
    assertEquals(from.directionTo(Position(3, 3)), None)
    assertEquals(from.directionTo(Position(3, 2)), None)
    assertEquals(from.directionTo(Position(3, 1)), Some(List(Direction.Down)))
    assertEquals(from.directionTo(Position(2, 5)), Some(List(Direction.Up, Direction.Left)))
    assertEquals(from.directionTo(Position(2, 4)), None)
    assertEquals(from.directionTo(Position(2, 3)), None)
    assertEquals(from.directionTo(Position(2, 2)), None)
    assertEquals(from.directionTo(Position(2, 1)), Some(List(Direction.Down, Direction.Left)))
    assertEquals(from.directionTo(Position(1, 5)), Some(List(Direction.Up, Direction.Left)))
    assertEquals(from.directionTo(Position(1, 4)), Some(List(Direction.Up, Direction.Left)))
    assertEquals(from.directionTo(Position(1, 3)), Some(List(Direction.Left)))
    assertEquals(from.directionTo(Position(1, 2)), Some(List(Direction.Down, Direction.Left)))
    assertEquals(from.directionTo(Position(1, 1)), Some(List(Direction.Down, Direction.Left)))
  }

  test("part 1") {
    assertEquals(Day09.part1(input), 13)
  }
}
