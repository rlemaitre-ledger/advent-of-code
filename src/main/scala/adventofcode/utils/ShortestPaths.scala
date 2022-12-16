package adventofcode.utils

import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.collection.*
import scala.collection.mutable.Map as MutableMap

case class ShortestPaths[Vertex, Cost](from: Set[Vertex], directPaths: Map[Vertex, Path[Vertex, Cost]]) {
  def cost(vertex: Vertex): Cost = directPaths(vertex).cost
  def pathTo(vertex: Vertex): List[Edge[Vertex, Cost]] = {
    @tailrec
    def construct(v: Vertex, acc: List[Edge[Vertex, Cost]]): List[Edge[Vertex, Cost]] = {
      directPaths.get(v) match
        case None => acc
        case Some(value) => {
          (value.predecessor, value.edge) match
            case (Some(predecessor), Some(edge)) if predecessor != v => construct(predecessor, edge :: acc)
            case _                                                   => acc
        }
    }
    construct(vertex, Nil)
  }
}

object ShortestPaths {
  def from[Vertex, Cost](
      from: Vertex,
      adjacency: Map[Vertex, List[Edge[Vertex, Cost]]]
  )(implicit n: Numeric[Cost]): ShortestPaths[Vertex, Cost] =
    fromMultiple(Set(from), adjacency)

  @nowarn
  def fromMultiple[Vertex, Cost](
      from: Set[Vertex],
      adjacency: Map[Vertex, List[Edge[Vertex, Cost]]]
  )(implicit n: Numeric[Cost]): ShortestPaths[Vertex, Cost] =
    import n.*
    val distanceTo = MutableMap.empty[Vertex, Path[Vertex, Cost]]
    from.foreach(distanceTo.put(_, Path(None, n.zero, None)))
    val sortByDistance: Ordering[Path[Vertex, Cost]] = (p1, p2) => (p1.cost - p2.cost).toInt
    val initial                                      = from.map(c => Path(Some(c), n.zero, None)).toList
    val queue = mutable.PriorityQueue[Path[Vertex, Cost]](initial: _*)(sortByDistance)
    while (queue.nonEmpty)
      val p     = queue.dequeue()
      val edges = adjacency.getOrElse(p.predecessor.get, List.empty)
      edges.foreach { e =>
        val previousDistance = distanceTo(e.from)
        distanceTo.get(e.to) match
          case Some(path) if path.cost <= previousDistance.cost + e.cost => path
          case _ =>
            val path = Path(Some(e.from), previousDistance.cost + e.cost, Some(e))
            distanceTo.put(e.to, path)
            if (!queue.exists(_.predecessor.contains(e.to))) {
              queue.enqueue(Path(Some(e.to), path.cost, None))
            }
      }
    ShortestPaths(from, distanceTo.toMap)
}

case class Path[Vertex, Cost](predecessor: Option[Vertex], cost: Cost, edge: Option[Edge[Vertex, Cost]])
trait Edge[Vertex, Cost](val from: Vertex, val to: Vertex, val cost: Cost)
