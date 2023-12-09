package adventofcode.utils

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.math.Ordered.orderingToOrdered

object graph:
  case class Graph[Node, E <: Edge[Node, Weight], Weight](nodes: Set[Node], edges: Set[E])(using num: Numeric[Weight]):
    val roots: Set[Node]                            = nodes.filter(incoming(_).isEmpty)
    def addNode(node: Node): Graph[Node, E, Weight] = copy(nodes = nodes + node)
    def addEdge(edge: E): Graph[Node, E, Weight]    = copy(edges = edges + edge)
    def neighbors(node: Node): Set[Node] =
      edges.filter(_.from == node).map(_.to) ++ edges.filter(e => !e.isDirected && e.to == node).map(_.from)
    def ancestors(node: Node): Set[Node] =
      edges.filter(_.to == node).map(_.from) ++ edges.filter(e => !e.isDirected && e.from == node).map(_.to)

    def incoming(node: Node): Map[Node, Weight] =
      (edges.filter(_.to == node).map(e => e.from -> e.weight) ++
        edges.filter(e => !e.isDirected && e.from == node).map(e => e.to -> e.weight)).toMap
    def outgoing(node: Node): Map[Node, Weight] =
      (edges.filter(_.from == node).map(e => e.to -> e.weight) ++
        edges.filter(e => !e.isDirected && e.to == node).map(e => e.from -> e.weight)).toMap

    def reachableFrom(start: Node): Set[Node] =
      @tailrec
      def bfsImpl(queue: Queue[Node], visited: Set[Node]): Set[Node] =
        if queue.isEmpty then visited
        else
          val (node, rest) = queue.dequeue
          val newVisited   = visited + node
          val newQueue     = rest ++ neighbors(node).diff(newVisited)
          bfsImpl(newQueue, newVisited)
      bfsImpl(Queue(start), Set.empty)
    lazy val isCycle: Boolean =
      @tailrec
      def dfsImpl(visited: Set[Node], stack: List[Node]): Boolean =
        if stack.isEmpty then true
        else
          val node :: rest = stack
          if visited.contains(node) then false
          else
            val newStack   = rest.prependedAll(neighbors(node))
            val newVisited = visited + node
            dfsImpl(newVisited, newStack)
      roots.isEmpty || roots.exists(n => dfsImpl(Set.empty, List(n)))

    lazy val topologicalSort: List[Node] =
      @tailrec
      def dfsImpl(visited: Set[Node], stack: List[Node]): List[Node] =
        if stack.isEmpty then visited.toList
        else
          val node :: rest = stack
          if visited.contains(node) then dfsImpl(visited, rest)
          else
            val newStack   = rest.prependedAll(neighbors(node))
            val newVisited = visited + node
            dfsImpl(newVisited, newStack)
      if roots.isEmpty then Nil else dfsImpl(roots, roots.toList)

    def shortestPath(from: Node, to: Node): Option[Path[Node, Weight]] = shortestPath(Set(from)).get(to)
    def shortestPath(start: Set[Node]): Map[Node, Path[Node, Weight]] =
      @tailrec
      def dijkstra(
          minHeap: Map[Node, Path[Node, Weight]],
          paths: Map[Node, Path[Node, Weight]]
      ): Map[Node, Path[Node, Weight]] =
        if minHeap.isEmpty then paths
        else
          val (node, path) = minHeap.minBy(_._2.weight)
          val newMinHeap   = minHeap - node
          val newPaths     = paths.updated(node, path)
          val newMinHeap2 = outgoing(node)
            .filterNot((n, _) => newPaths.contains(n))
            .foldLeft(newMinHeap):
              case (heap, (neighbor, weight)) =>
                val newPath = path.add(neighbor, weight)
                heap.get(neighbor) match
                  case Some(p) if p.weight <= newPath.weight => heap
                  case _                                     => heap.updated(neighbor, newPath)
          dijkstra(newMinHeap2, newPaths)
      val minHeap: Map[Node, Path[Node, Weight]] = start.map(n => n -> Path.empty).toMap
      dijkstra(minHeap, minHeap)

    lazy val shortestPaths: Map[Node, Path[Node, Weight]] = shortestPath(roots)

  object Graph:
    def empty[Node, E <: Edge[Node, Weight], Weight: Numeric]: Graph[Node, E, Weight] = Graph(Set.empty, Set.empty)
    def of[Node, E <: Edge[Node, Weight], Weight: Numeric](edges: Iterable[E]): Graph[Node, E, Weight] =
      Graph(edges.flatMap(e => List(e.from, e.to)).toSet, edges.toSet)
  final case class Path[Node, Weight](private val fromEnd: List[Node], private val weights: List[Weight])(using
      num: Numeric[Weight]
  ):
    val weight: Weight    = weights.sum
    val nodes: List[Node] = fromEnd.reverse
    def add(node: Node, weight: Weight): Path[Node, Weight] =
      copy(fromEnd = node :: fromEnd, weights = weight :: weights)
  object Path:
    def empty[Node, Weight: Numeric]: Path[Node, Weight] = Path(Nil, Nil)
    def apply[Node, Weight](edge: Edge[Node, Weight])(using num: Numeric[Weight]): Path[Node, Weight] =
      Path(List(edge.to, edge.from), List(edge.weight))

  sealed trait Edge[Node, Weight](using num: Numeric[Weight]):
    def from: Node
    def to: Node
    def weight: Weight = num.one
    def isDirected: Boolean
  trait DirectedEdge[Node, Weight] extends Edge[Node, Weight]:
    final override def isDirected: Boolean = true
  trait UndirectedEdge[Node, Weight] extends Edge[Node, Weight]:
    final override def isDirected: Boolean = false
