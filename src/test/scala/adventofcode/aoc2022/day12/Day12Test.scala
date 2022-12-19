package adventofcode.aoc2022.day12

import adventofcode.*
import adventofcode.aoc2022.day12.Day12
import adventofcode.aoc2022.day12.Day12.*

class Day12Test extends AoCTest {
  override val lines: String = """Sabqponm
                                 |abcryxxl
                                 |accszExk
                                 |acctuvwj
                                 |abdefghi""".stripMargin
  val map: HeightMap = HeightMap(
    List(
      List(
        Cell(Coordinates(0, 0), Altitude('a', CellType.Start)),
        Cell(Coordinates(0, 1), Altitude('a', CellType.Normal)),
        Cell(Coordinates(0, 2), Altitude('b', CellType.Normal)),
        Cell(Coordinates(0, 3), Altitude('q', CellType.Normal)),
        Cell(Coordinates(0, 4), Altitude('p', CellType.Normal)),
        Cell(Coordinates(0, 5), Altitude('o', CellType.Normal)),
        Cell(Coordinates(0, 6), Altitude('n', CellType.Normal)),
        Cell(Coordinates(0, 7), Altitude('m', CellType.Normal))
      ),
      List(
        Cell(Coordinates(1, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(1, 1), Altitude('b', CellType.Normal)),
        Cell(Coordinates(1, 2), Altitude('c', CellType.Normal)),
        Cell(Coordinates(1, 3), Altitude('r', CellType.Normal)),
        Cell(Coordinates(1, 4), Altitude('y', CellType.Normal)),
        Cell(Coordinates(1, 5), Altitude('x', CellType.Normal)),
        Cell(Coordinates(1, 6), Altitude('x', CellType.Normal)),
        Cell(Coordinates(1, 7), Altitude('l', CellType.Normal))
      ),
      List(
        Cell(Coordinates(2, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(2, 1), Altitude('c', CellType.Normal)),
        Cell(Coordinates(2, 2), Altitude('c', CellType.Normal)),
        Cell(Coordinates(2, 3), Altitude('s', CellType.Normal)),
        Cell(Coordinates(2, 4), Altitude('z', CellType.Normal)),
        Cell(Coordinates(2, 5), Altitude('z', CellType.End)),
        Cell(Coordinates(2, 6), Altitude('x', CellType.Normal)),
        Cell(Coordinates(2, 7), Altitude('k', CellType.Normal))
      ),
      List(
        Cell(Coordinates(3, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(3, 1), Altitude('c', CellType.Normal)),
        Cell(Coordinates(3, 2), Altitude('c', CellType.Normal)),
        Cell(Coordinates(3, 3), Altitude('t', CellType.Normal)),
        Cell(Coordinates(3, 4), Altitude('u', CellType.Normal)),
        Cell(Coordinates(3, 5), Altitude('v', CellType.Normal)),
        Cell(Coordinates(3, 6), Altitude('w', CellType.Normal)),
        Cell(Coordinates(3, 7), Altitude('j', CellType.Normal))
      ),
      List(
        Cell(Coordinates(4, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(4, 1), Altitude('b', CellType.Normal)),
        Cell(Coordinates(4, 2), Altitude('d', CellType.Normal)),
        Cell(Coordinates(4, 3), Altitude('e', CellType.Normal)),
        Cell(Coordinates(4, 4), Altitude('f', CellType.Normal)),
        Cell(Coordinates(4, 5), Altitude('g', CellType.Normal)),
        Cell(Coordinates(4, 6), Altitude('h', CellType.Normal)),
        Cell(Coordinates(4, 7), Altitude('i', CellType.Normal))
      )
    )
  )
  val testInstance: Day12 = Day12(map)
  test("Parse map") {
    assertEquals(parse(input), map)
  }
  test("min distance") {
    assertEquals(map.minDistanceFromStart, Distance(31))
  }
  test("part 1") {
    assertEquals(testInstance.part1, 31)
  }
  test("part 2") {
    assertEquals(testInstance.part2, 29)
  }
  test("answer".ignore) {
    assertEquals(Day12.instance.run(Mode.Part1), 484)
    assertEquals(Day12.instance.run(Mode.Part2), 478)
  }
  test("lowest cells") {
    assertEquals(
      map.lowestCells.toSet,
      Set(
        Cell(Coordinates(0, 0), Altitude('a', CellType.Start)),
        Cell(Coordinates(0, 1), Altitude('a', CellType.Normal)),
        Cell(Coordinates(1, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(2, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(3, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(4, 0), Altitude('a', CellType.Normal))
      )
    )
  }
  test("possible starting cell") {
    assertEquals(
      map.possibleStartingPoints,
      Set(
        Cell(Coordinates(0, 1), Altitude('a', CellType.Normal)),
        Cell(Coordinates(1, 0), Altitude('a', CellType.Normal)),
        Cell(Coordinates(4, 0), Altitude('a', CellType.Normal))
      )
    )
  }
  test("starting points in answers") {
    assertEquals(
      parse(inputLines("2022/day12.txt")).possibleStartingPoints.size,
      53
    )
  }
  test("elevation difference") {
    assertEquals(Elevation(1000) - Elevation(300), Elevation(700))
  }
}
