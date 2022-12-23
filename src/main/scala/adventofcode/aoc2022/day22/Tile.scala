package adventofcode.aoc2022.day22

import adventofcode.utils.coordinates.Coordinates

final case class Tile(
    coordinates: Coordinates,
    wall: Boolean,
    var up: Coordinates = null,
    var down: Coordinates = null,
    var right: Coordinates = null,
    var left: Coordinates = null
)
object Tile {
  def parse(coordinates: Coordinates, char: Char): Option[Tile] = char match
    case '.' => Some(Tile(coordinates, false))
    case '#' => Some(Tile(coordinates, true))
    case _   => None

}
