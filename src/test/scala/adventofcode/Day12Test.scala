package adventofcode

import adventofcode.Day12.*

class Day12Test extends AoCTest {
  override val lines: String = """Sabqponm
                                 |abcryxxl
                                 |accszExk
                                 |acctuvwj
                                 |abdefghi""".stripMargin
  val map: HeightMap = HeightMap(
    List(
      List(
        Cell(Coordinates(LineNumber(0), ColumnNumber(0)), Altitude('a', CellType.Start)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(1)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(2)), Altitude('b', CellType.Normal)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(3)), Altitude('q', CellType.Normal)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(4)), Altitude('p', CellType.Normal)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(5)), Altitude('o', CellType.Normal)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(6)), Altitude('n', CellType.Normal)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(7)), Altitude('m', CellType.Normal))
      ),
      List(
        Cell(Coordinates(LineNumber(1), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(1)), Altitude('b', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(2)), Altitude('c', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(3)), Altitude('r', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(4)), Altitude('y', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(5)), Altitude('x', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(6)), Altitude('x', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(7)), Altitude('l', CellType.Normal))
      ),
      List(
        Cell(Coordinates(LineNumber(2), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(1)), Altitude('c', CellType.Normal)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(2)), Altitude('c', CellType.Normal)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(3)), Altitude('s', CellType.Normal)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(4)), Altitude('z', CellType.Normal)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(5)), Altitude('z', CellType.End)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(6)), Altitude('x', CellType.Normal)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(7)), Altitude('k', CellType.Normal))
      ),
      List(
        Cell(Coordinates(LineNumber(3), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(1)), Altitude('c', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(2)), Altitude('c', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(3)), Altitude('t', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(4)), Altitude('u', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(5)), Altitude('v', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(6)), Altitude('w', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(7)), Altitude('j', CellType.Normal))
      ),
      List(
        Cell(Coordinates(LineNumber(4), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(1)), Altitude('b', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(2)), Altitude('d', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(3)), Altitude('e', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(4)), Altitude('f', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(5)), Altitude('g', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(6)), Altitude('h', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(7)), Altitude('i', CellType.Normal))
      )
    )
  )
  test("Parse map") {
    assertEquals(parse(input), map)
  }
  test("min distance") {
    assertEquals(map.minDistanceFromStart, 31)
  }
  test("part 1") {
    assertEquals(part1(input), 31)
  }
  test("part 2") {
    assertEquals(part2(input), 29)
  }
  test("answer") {
    assertEquals(run(Mode.Part1), 484)
  }
  test("lowest cells") {
    assertEquals(
      map.lowestCells.toSet,
      Set(
        Cell(Coordinates(LineNumber(0), ColumnNumber(0)), Altitude('a', CellType.Start)),
        Cell(Coordinates(LineNumber(0), ColumnNumber(1)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(2), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(3), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(0)), Altitude('a', CellType.Normal))
      )
    )
  }
  test("possible starting cell") {
    assertEquals(
      map.possibleStartingPoints,
      Set(
        Cell(Coordinates(LineNumber(0), ColumnNumber(1)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(1), ColumnNumber(0)), Altitude('a', CellType.Normal)),
        Cell(Coordinates(LineNumber(4), ColumnNumber(0)), Altitude('a', CellType.Normal))
      )
    )
  }
  test("starting points in answers") {
    assertEquals(
      parse(Day12.input).possibleStartingPoints.size,
      53
    )
  }
}
