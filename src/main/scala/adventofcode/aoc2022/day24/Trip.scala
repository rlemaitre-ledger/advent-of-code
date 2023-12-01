package adventofcode.aoc2022.day24

import adventofcode.utils.coordinates.Coordinates

final case class Trip (valley: Valley, position: Coordinates, minutes: Int) {
  def atEnd: Boolean = position == valley.exit
  def next: Trip =
    val nextValley = valley.next
    val possiblePositions = List(
      position,
      position + Coordinates.north,
      position + Coordinates.south,
      position + Coordinates.east,
      position + Coordinates.west,
    )
    val inValley = possiblePositions
      .filter(p => (0 until valley.nbLines).contains(p.y) && (0 until valley.nbColumn).contains(p.x))
    val emptyPositions = inValley
      .filter(p => nextValley.tiles(p).isEmpty)
    val nextPosition = emptyPositions
      .map(p => (p, p.manhattanDistance(valley.exit)))
      .minBy(_._2)._1
    copy(
      valley = nextValley,
      position = nextPosition,
      minutes =  minutes + 1
    )
}


object Trip {
  def parse(lines: List[String]): Trip = {
    val valley = Valley.parse(lines)
    Trip(valley, valley.entry, 0)
  }
}
