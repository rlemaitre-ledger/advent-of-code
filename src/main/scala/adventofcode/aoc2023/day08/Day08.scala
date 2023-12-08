package adventofcode.aoc2023.day08
import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.coordinates.Direction
import scala.annotation.tailrec

final case class Day08(input: List[String]) extends Problem[List[String], BigInt, BigInt](2023, 8, "Haunted Wasteland"):
  override def part1: BigInt = Network.parse(input).run
  override def part2: BigInt = Network.parse(input).ghosts

object Day08:
  val instance: Day08 = Day08(inputLines("2023/day08.txt"))

case class Node(
    name: String,
    leftName: String,
    rightName: String,
    var left: Node = Node.unknown,
    var right: Node = Node.unknown
)

object Node:
  val unknown: Node = Node("", "", "")

case class Network(nodes: Map[String, Node], directions: List[Direction]):

  private def walk(start: Node, predicate: Node => Boolean): BigInt =
    @tailrec
    def go(current: Node, directions: Iterator[Direction], steps: BigInt): BigInt =
      if predicate(current) then steps
      else
        directions.next() match
          case Direction.Left  => go(current.left, directions, steps + 1)
          case Direction.Right => go(current.right, directions, steps + 1)
          case _               => throw RuntimeException("Not possible")

    go(start, Iterator.continually(directions).flatten, BigInt(0))

  def run: BigInt = walk(nodes("AAA"), _.name == "ZZZ")
  def ghosts: BigInt =
    val starts: List[Node] = nodes.filter(_._1.endsWith("A")).values.toList

    @tailrec
    def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)
    def lcm(list: Seq[BigInt]): BigInt    = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)
    lcm(starts.map(n => walk(n, _.name.endsWith("Z"))))
object Network:
  def parse(lines: List[String]): Network =
    val directions = lines.head.map(_.toString).map(Direction.parse).toList
    val nodes =
      val regexp = raw"^(...) = \((...), (...)\)".r
      lines
        .drop(2)
        .map:
          case regexp(name, left, right) => name -> Node(name, left, right)
        .toMap
    nodes.foreach: (name, node) =>
      node.left = nodes(node.leftName)
      node.right = nodes(node.rightName)
    Network(nodes, directions)
