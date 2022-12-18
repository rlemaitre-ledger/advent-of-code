package adventofcode.day18

final case class Coordinates3D(x: Int, y: Int, z: Int) {
  def visibleSides(cubes: Set[Coordinates3D]): Int = 6 - (cubes - this).count { isAdjacent }
  def isAdjacent(other: Coordinates3D): Boolean = {
    val Coordinates3D(x1, y1, z1) = other
    Math.abs(x - x1) + Math.abs(y - y1) + Math.abs(z - z1) == 1
  }
  def neighbours: Set[Coordinates3D] = Set(
    plus(Coordinates3D(1, 0, 0)),
    plus(Coordinates3D(-1, 0, 0)),
    plus(Coordinates3D(0, 1, 0)),
    plus(Coordinates3D(0, -1, 0)),
    plus(Coordinates3D(0, 0, 1 )),
    plus(Coordinates3D(0, 0,-1 ))
  )
  def plus(other: Coordinates3D): Coordinates3D = copy(x = x + other.x, y = y + other.y, z = z + other.z)
  def in(box: Box): Boolean = box.x.contains(x) && box.y.contains(y) && box.z.contains(z)
}
object Coordinates3D {
  def fromString(str: String): Coordinates3D = {
    val parts = str.split(",").map(_.toInt)
    Coordinates3D(parts(0), parts(1), parts(2))
  }
  def fromLines(lines: List[String]): Set[Coordinates3D] = lines.map(fromString).toSet
}
