package adventofcode.day18

import scala.annotation.nowarn

final case class Box(particles: Set[Coordinates3D], x: Range, y: Range, z: Range) {
  val min: Coordinates3D = Coordinates3D(x.head, y.head, z.head)
  @nowarn
  def outsideCubes: Set[Coordinates3D] = {
    val start   = min
    val queue   = collection.mutable.Queue(start)
    val outside = collection.mutable.Set(start)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      val toVisit = current.neighbours.diff(particles).diff(outside)
      toVisit.foreach { next =>
        if (next.in(this)) {
          queue.enqueue(next)
          outside += next
        }
      }
    }
    outside.toSet
  }
}
object Box {
  def enclosing(cubes: Set[Coordinates3D]): Box = {
    def min(f: Coordinates3D => Int)          = cubes.map(f).min
    def max(f: Coordinates3D => Int)          = cubes.map(f).max
    def range(f: Coordinates3D => Int): Range = min(f) - 1 to max(f) + 1
    Box(cubes, range(_.x), range(_.y), range(_.z))
  }
}
