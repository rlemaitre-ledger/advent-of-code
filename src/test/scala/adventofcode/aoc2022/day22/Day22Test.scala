package adventofcode.aoc2022.day22

import adventofcode.AoCTest
import adventofcode.aoc2022.day22.Day22.*
import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Direction

class Day22Test extends AoCTest {
  override val lines: String =
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5""".stripMargin
  test("parse board") {
    val board = Board.parse(input.dropRight(2))
    assertEquals(board.tiles(Coordinates(9, 11)).left, Coordinates(16, 11))

  }
  test("part 1") {
    val board = parse(input)
    assertEquals(Day22(board).part1, 6032)
  }
  test("part 2") {
    intercept[NotImplementedError] {
      Day22(parse(input)).part2
    }
  }
  test("move") {
    val game = parse(input)
    assertEquals(game.position, Coordinates(9, 1))
    assertEquals(game.direction, Direction.Right)
    val next = game.next
    assertEquals(next.position, Coordinates(11, 1))
    assertEquals(next.direction, Direction.Right)
    val next2 = next.next
    assertEquals(next2.position, Coordinates(11, 1))
    assertEquals(next2.direction, Direction.Down)
    val next3 = next2.next
    assertEquals(next3.position, Coordinates(11, 6))
    assertEquals(next3.direction, Direction.Down)
    val game2 = Game(
      List(Move.TurnLeft, Move.TurnLeft, Move.TurnLeft, Move.TurnLeft),
      Board.parse(input.dropRight(2)),
      Direction.Right,
      Coordinates(9, 1)
    )
    assertEquals(game2.next.direction, Direction.Up)
    assertEquals(game2.next.next.direction, Direction.Left)
    assertEquals(game2.next.next.next.direction, Direction.Down)
    assertEquals(game2.next.next.next.next.direction, Direction.Right)
    val game3 = Game(
      List(Move.TurnRight, Move.TurnRight, Move.TurnRight, Move.TurnRight),
      Board.parse(input.dropRight(2)),
      Direction.Right,
      Coordinates(9, 1)
    )
    assertEquals(game3.next.direction, Direction.Down)
    assertEquals(game3.next.next.direction, Direction.Left)
    assertEquals(game3.next.next.next.direction, Direction.Up)
    assertEquals(game3.next.next.next.next.direction, Direction.Right)
  }
  test("go forward") {
    val game = Game(List(Move.Forward(2)), Board.parse(input.dropRight(2)), Direction.Right, Coordinates(4, 6))
    assertEquals(game.next.position, Coordinates(6, 6))
    val game2 = Game(List(Move.Forward(2)), Board.parse(input.dropRight(2)), Direction.Up, Coordinates(10, 5))
    assertEquals(game2.next.position, Coordinates(10, 3))
    val game3 = Game(List(Move.Forward(2)), Board.parse(input.dropRight(2)), Direction.Left, Coordinates(4, 6))
    assertEquals(game3.next.position, Coordinates(2, 6))
    val game4 = Game(List(Move.Forward(2)), Board.parse(input.dropRight(2)), Direction.Down, Coordinates(10, 5))
    assertEquals(game4.next.position, Coordinates(10, 7))
  }
}
