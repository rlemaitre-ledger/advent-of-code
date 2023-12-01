package adventofcode.aoc2022.day24

import adventofcode.utils.coordinates.Direction

enum Tile {
  case Wall
  case Wind(direction: Direction)
}
object Tile {
  def parse(char: Char): Set[Tile] = char match
    case '#' => Set(Wall)
    case '>' => Set(Wind(Direction.Right))
    case '<' => Set(Wind(Direction.Left))
    case '^' => Set(Wind(Direction.Up))
    case 'v' => Set(Wind(Direction.Down))
    case _   => Set.empty
}
