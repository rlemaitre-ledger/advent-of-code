package adventofcode.day17

import adventofcode.Coordinates
import adventofcode.Direction

case class Rock(name: String, points: Set[Coordinates]) {
  def move(vector: Coordinates): Rock = copy(points = points.map(_.plus(vector)))
  def canMove(grid: Grid): Boolean    = points.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p))
}
object Rock {
  val all: Seq[Rock] = Seq(
    Rock("horizontal bar", Set(Coordinates(0, 0), Coordinates(1, 0), Coordinates(2, 0), Coordinates(3, 0))),
    Rock("cross", Set(Coordinates(1, 0), Coordinates(0, 1), Coordinates(1, 1), Coordinates(2, 1), Coordinates(1, 2))),
    Rock("corner", Set(Coordinates(0, 0), Coordinates(1, 0), Coordinates(2, 0), Coordinates(2, 1), Coordinates(2, 2))),
    Rock("vertical bar", Set(Coordinates(0, 0), Coordinates(0, 1), Coordinates(0, 2), Coordinates(0, 3))),
    Rock("square", Set(Coordinates(0, 0), Coordinates(1, 0), Coordinates(0, 1), Coordinates(1, 1)))
  )
}
