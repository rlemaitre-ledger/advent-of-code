import scala.annotation.nowarn
import scala.io.Source

package object adventofcode:
  def inputLines(path: String): List[String] = Source
    .fromResource(path)
    .getLines()
    .toList

  def time[A](f: => A): A = {
    val s   = System.nanoTime
    val ret = f
    println("time: " + (System.nanoTime - s) / 1e6 + "ms")
    ret
  }
  enum Mode:
    case Part1, Part2

  def bfs[A](start: A, graph: A => Seq[A]): Map[A, Int] = {
    val todo = collection.mutable.Queue(start)
    val cost = collection.mutable.Map(start -> 1)

    while todo.nonEmpty do
      val current = todo.dequeue()
      graph(current).filterNot(cost.contains).foreach { next =>
        todo.enqueue(next): @nowarn
        cost(next) = cost(current) + 1
      }

    cost.toMap
  }
