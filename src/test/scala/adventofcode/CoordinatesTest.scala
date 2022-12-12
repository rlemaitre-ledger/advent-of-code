package adventofcode

import munit.FunSuite
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import scala.annotation.nowarn

class CoordinatesTest extends FunSuite, ScalaCheckSuite {
  test("Direction to") {
    val from = Coordinates(3, 3)
    assertEquals(from.directionTo(Coordinates(5, 5)), Some(List(Direction.Up, Direction.Right)))
    assertEquals(from.directionTo(Coordinates(5, 4)), Some(List(Direction.Up, Direction.Right)))
    assertEquals(from.directionTo(Coordinates(5, 3)), Some(List(Direction.Right)))
    assertEquals(from.directionTo(Coordinates(5, 2)), Some(List(Direction.Down, Direction.Right)))
    assertEquals(from.directionTo(Coordinates(5, 1)), Some(List(Direction.Down, Direction.Right)))
    assertEquals(from.directionTo(Coordinates(4, 5)), Some(List(Direction.Up, Direction.Right)))
    assertEquals(from.directionTo(Coordinates(4, 4)), None)
    assertEquals(from.directionTo(Coordinates(4, 3)), None)
    assertEquals(from.directionTo(Coordinates(4, 2)), None)
    assertEquals(from.directionTo(Coordinates(4, 1)), Some(List(Direction.Down, Direction.Right)))
    assertEquals(from.directionTo(Coordinates(3, 5)), Some(List(Direction.Up)))
    assertEquals(from.directionTo(Coordinates(3, 4)), None)
    assertEquals(from.directionTo(Coordinates(3, 3)), None)
    assertEquals(from.directionTo(Coordinates(3, 2)), None)
    assertEquals(from.directionTo(Coordinates(3, 1)), Some(List(Direction.Down)))
    assertEquals(from.directionTo(Coordinates(2, 5)), Some(List(Direction.Up, Direction.Left)))
    assertEquals(from.directionTo(Coordinates(2, 4)), None)
    assertEquals(from.directionTo(Coordinates(2, 3)), None)
    assertEquals(from.directionTo(Coordinates(2, 2)), None)
    assertEquals(from.directionTo(Coordinates(2, 1)), Some(List(Direction.Down, Direction.Left)))
    assertEquals(from.directionTo(Coordinates(1, 5)), Some(List(Direction.Up, Direction.Left)))
    assertEquals(from.directionTo(Coordinates(1, 4)), Some(List(Direction.Up, Direction.Left)))
    assertEquals(from.directionTo(Coordinates(1, 3)), Some(List(Direction.Left)))
    assertEquals(from.directionTo(Coordinates(1, 2)), Some(List(Direction.Down, Direction.Left)))
    assertEquals(from.directionTo(Coordinates(1, 1)), Some(List(Direction.Down, Direction.Left)))
  }
  property("coordinates is always adjacent to itself") {
    forAll { (x: Int, y: Int) =>
      Coordinates(x, y).adjacentTo(Coordinates(x, y))
    }
  }
  property("adjacent coordinates") {
    forAll { (x: Int, y: Int) =>
      Coordinates(x, y).adjacentTo(Coordinates(x + 1, y - 1)): @nowarn
      Coordinates(x, y).adjacentTo(Coordinates(x + 1, y)): @nowarn
      Coordinates(x, y).adjacentTo(Coordinates(x + 1, y + 1)): @nowarn
      Coordinates(x, y).adjacentTo(Coordinates(x - 1, y - 1)): @nowarn
      Coordinates(x, y).adjacentTo(Coordinates(x - 1, y)): @nowarn
      Coordinates(x, y).adjacentTo(Coordinates(x - 1, y + 1)): @nowarn
      Coordinates(x, y).adjacentTo(Coordinates(x, y - 1)): @nowarn
      Coordinates(x, y).adjacentTo(Coordinates(x, y + 1)): @nowarn

      !Coordinates(x, y).adjacentTo(Coordinates(x + 2, y - 2)): @nowarn
      !Coordinates(x, y).adjacentTo(Coordinates(x + 2, y)): @nowarn
      !Coordinates(x, y).adjacentTo(Coordinates(x + 2, y + 2)): @nowarn
      !Coordinates(x, y).adjacentTo(Coordinates(x - 2, y - 2)): @nowarn
      !Coordinates(x, y).adjacentTo(Coordinates(x - 2, y)): @nowarn
      !Coordinates(x, y).adjacentTo(Coordinates(x - 2, y + 2)): @nowarn
      !Coordinates(x, y).adjacentTo(Coordinates(x, y - 2)): @nowarn
      !Coordinates(x, y).adjacentTo(Coordinates(x, y + 2)): @nowarn
    }
  }
  property("adjacency is commutative") {
    forAll { (x1: Int, y1: Int, x2: Int, y2: Int) =>
      Coordinates(x1, y1).adjacentTo(Coordinates(x2, y2)) == Coordinates(x2, y2).adjacentTo(Coordinates(x1, y1))
    }
  }

}
