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
  val longerInput: List[String] =
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20""".stripMargin.split('\n').toList

  val simpleInstance: Day09 = Day09(moves(input))
  val longInstance: Day09   = Day09(moves(longerInput))

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

  test("state move head") {
    val state  = State.start
    val state1 = state.moveHead(Direction.Right)
    assertEquals(state1.head.current, Coordinates(1, 0))
    assertEquals(state1.tail.last.current, Coordinates(0, 0))
    assertEquals(state1.head.past, Set(Coordinates.origin, Coordinates(1, 0)))
    assertEquals(state1.tail.last.past, Set(Coordinates.origin))
    val state2 = state1.moveHead(Direction.Right)
    assertEquals(state2.head.current, Coordinates(2, 0))
    assertEquals(state2.tail.last.current, Coordinates(1, 0))
    assertEquals(state2.head.past, Set(Coordinates.origin, Coordinates(1, 0), Coordinates(2, 0)))
    assertEquals(state2.tail.last.past, Set(Coordinates.origin, Coordinates(1, 0)))
    val state3 = state2.moveHead(Direction.Right)
    assertEquals(state3.head.current, Coordinates(3, 0))
    assertEquals(state3.tail.last.current, Coordinates(2, 0))
    assertEquals(state3.head.past, Set(Coordinates.origin, Coordinates(1, 0), Coordinates(2, 0), Coordinates(3, 0)))
    assertEquals(state3.tail.last.past, Set(Coordinates.origin, Coordinates(1, 0), Coordinates(2, 0)))
    val state4 = state3.moveHead(Direction.Right)
    assertEquals(state4.head.current, Coordinates(4, 0))
    assertEquals(state4.tail.last.current, Coordinates(3, 0))
    assertEquals(
      state4.head.past,
      Set(Coordinates.origin, Coordinates(1, 0), Coordinates(2, 0), Coordinates(3, 0), Coordinates(4, 0))
    )
    assertEquals(
      state4.tail.last.past,
      Set(Coordinates.origin, Coordinates(1, 0), Coordinates(2, 0), Coordinates(3, 0))
    )
    val state5 = state4.moveHead(Direction.Up)
    assertEquals(state5.head.current, Coordinates(4, 1))
    assertEquals(state5.tail.last.current, Coordinates(3, 0))
    assertEquals(
      state5.head.past,
      Set(
        Coordinates.origin,
        Coordinates(1, 0),
        Coordinates(2, 0),
        Coordinates(3, 0),
        Coordinates(4, 0),
        Coordinates(4, 1)
      )
    )
    assertEquals(
      state5.tail.last.past,
      Set(Coordinates.origin, Coordinates(1, 0), Coordinates(2, 0), Coordinates(3, 0))
    )
    val state6 = state5.moveHead(Direction.Up)
    assertEquals(state6.head.current, Coordinates(4, 2))
    assertEquals(state6.tail.last.current, Coordinates(4, 1))
    assertEquals(
      state6.head.past,
      Set(
        Coordinates.origin,
        Coordinates(1, 0),
        Coordinates(2, 0),
        Coordinates(3, 0),
        Coordinates(4, 0),
        Coordinates(4, 1),
        Coordinates(4, 2)
      )
    )
    assertEquals(
      state6.tail.last.past,
      Set(Coordinates.origin, Coordinates(1, 0), Coordinates(2, 0), Coordinates(3, 0), Coordinates(4, 1))
    )
  }
  test("part 1") {
    assertEquals(simpleInstance.part1, 13)
  }
  test("part 2") {
    assertEquals(simpleInstance.part2, 1)
    assertEquals(longInstance.part2, 36)
  }
  test("answers") {
    assertEquals(Day09.instance.run(Mode.Part1), 6212)
    assertEquals(Day09.instance.run(Mode.Part2), 2522)
  }
}
