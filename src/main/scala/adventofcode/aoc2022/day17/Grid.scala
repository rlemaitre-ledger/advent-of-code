package adventofcode.aoc2022.day17

import adventofcode.utils.coordinates.Coordinates

final case class Grid(private val points: Set[Coordinates]) {
  def add(rock: Rock): Grid            = copy(points = points ++ rock.points)
  def contains: Coordinates => Boolean = points.contains
}

object Grid {
  val empty: Grid = Grid(Set.tabulate(8)(Coordinates(_, 0)))
}
