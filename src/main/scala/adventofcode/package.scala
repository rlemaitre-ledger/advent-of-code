import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.io.Source

package object adventofcode:
  def inputLines(path: String): List[String] = Source
    .fromResource(path)
    .getLines()
    .toList

  def time[A](f: => A): A =
    val s   = System.nanoTime
    val ret = f
    println("time: " + (System.nanoTime - s) / 1e6 + "ms")
    ret
  enum Mode:
    case Part1, Part2

  def bfs[A](start: A, graph: A => Seq[A], startCost: Int = 1): Map[A, Int] =
    val todo = collection.mutable.Queue(start)
    val cost = collection.mutable.Map(start -> startCost)

    while todo.nonEmpty do
      val current = todo.dequeue()
      graph(current).filterNot(cost.contains).foreach { next =>
        todo.enqueue(next): @nowarn
        cost(next) = cost(current) + 1
      }
    cost.toMap
  def dfs[A](start: A, graph: A => Seq[A], startCost: Int = 1): Map[A, Int] =
    @tailrec
    def dfsImpl(visited: Map[A, Int], stack: List[(A, Int)]): Map[A, Int] =
      if stack.isEmpty then visited
      else
        val (node, cost) :: rest = stack
        if visited.contains(node) then dfsImpl(visited, rest)
        else
          val newStack   = rest.prependedAll(graph(node).map(n => n -> (cost + 1)))
          val newVisited = visited + (node -> cost)
          dfsImpl(newVisited, newStack)

    dfsImpl(Map.empty, List(start -> startCost))
