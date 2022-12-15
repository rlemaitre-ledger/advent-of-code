package adventofcode

import munit.FunSuite
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.annotation.nowarn

class CoordinatesTest extends FunSuite, ScalaCheckSuite {
  test("Direction to") {
    val from = Coordinates(3, 3)
    assertEquals(from.directionTo(Coordinates(5, 5)), List(Direction.Up, Direction.Right))
    assertEquals(from.directionTo(Coordinates(5, 4)), List(Direction.Up, Direction.Right))
    assertEquals(from.directionTo(Coordinates(5, 3)), List(Direction.Right))
    assertEquals(from.directionTo(Coordinates(5, 2)), List(Direction.Down, Direction.Right))
    assertEquals(from.directionTo(Coordinates(5, 1)), List(Direction.Down, Direction.Right))
    assertEquals(from.directionTo(Coordinates(4, 5)), List(Direction.Up, Direction.Right))
    assertEquals(from.directionTo(Coordinates(4, 4)), Nil)
    assertEquals(from.directionTo(Coordinates(4, 3)), Nil)
    assertEquals(from.directionTo(Coordinates(4, 2)), Nil)
    assertEquals(from.directionTo(Coordinates(4, 1)), List(Direction.Down, Direction.Right))
    assertEquals(from.directionTo(Coordinates(3, 5)), List(Direction.Up))
    assertEquals(from.directionTo(Coordinates(3, 4)), Nil)
    assertEquals(from.directionTo(Coordinates(3, 3)), Nil)
    assertEquals(from.directionTo(Coordinates(3, 2)), Nil)
    assertEquals(from.directionTo(Coordinates(3, 1)), List(Direction.Down))
    assertEquals(from.directionTo(Coordinates(2, 5)), List(Direction.Up, Direction.Left))
    assertEquals(from.directionTo(Coordinates(2, 4)), Nil)
    assertEquals(from.directionTo(Coordinates(2, 3)), Nil)
    assertEquals(from.directionTo(Coordinates(2, 2)), Nil)
    assertEquals(from.directionTo(Coordinates(2, 1)), List(Direction.Down, Direction.Left))
    assertEquals(from.directionTo(Coordinates(1, 5)), List(Direction.Up, Direction.Left))
    assertEquals(from.directionTo(Coordinates(1, 4)), List(Direction.Up, Direction.Left))
    assertEquals(from.directionTo(Coordinates(1, 3)), List(Direction.Left))
    assertEquals(from.directionTo(Coordinates(1, 2)), List(Direction.Down, Direction.Left))
    assertEquals(from.directionTo(Coordinates(1, 1)), List(Direction.Down, Direction.Left))
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
  property("Manhattan distance") {
    val gen = for {
      directions <- Gen.oneOf(
        Gen.containerOf[List, Direction](Gen.oneOf(Direction.Up, Direction.Left)),
        Gen.containerOf[List, Direction](Gen.oneOf(Direction.Up, Direction.Right)),
        Gen.containerOf[List, Direction](Gen.oneOf(Direction.Down, Direction.Left)),
        Gen.containerOf[List, Direction](Gen.oneOf(Direction.Down, Direction.Right)),
        Gen.containerOf[List, Direction](Gen.const(Direction.Up)),
        Gen.containerOf[List, Direction](Gen.const(Direction.Right)),
        Gen.containerOf[List, Direction](Gen.const(Direction.Left)),
        Gen.containerOf[List, Direction](Gen.const(Direction.Down))
      )
      x <- Arbitrary.arbitrary[Int]
      y <- Arbitrary.arbitrary[Int]
    } yield (Coordinates(x, y), directions)
    forAll(gen) { (coordinates: Coordinates, directions: List[Direction]) =>
      val point = directions.foldLeft(coordinates) { (c, dir) => c.move(dir) }
      assertEquals(coordinates.manhattanDistance(point), directions.size)
    }
  }
}
