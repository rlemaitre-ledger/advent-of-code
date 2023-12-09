package adventofcode.aoc2023.day08
import adventofcode.Problem
import adventofcode.inputLines
import adventofcode.utils.coordinates.Direction
import adventofcode.utils.math.lcm
import scala.annotation.tailrec

final case class Day08(input: List[String], simple: Boolean = false) extends Problem[List[String], BigInt, BigInt](2023, 8, "Haunted Wasteland"):
  override def part1: BigInt = Network.parse(input).run
  override def part2: BigInt = Network.parse(input).ghosts(simple)

object Day08:
  val instance: Day08 = Day08(inputLines("2023/day08.txt"))

case class Node(
    name: String,
    leftName: String,
    rightName: String,
    var left: Node = Node.unknown,
    var right: Node = Node.unknown
) extends Equals:
  override def toString: String = s"$name: ($leftName, $rightName)"
  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Node]

  // Intentionally avoiding the call to super.equals because no ancestor has overridden equals (see note 7 below)
  override def equals(that: Any): Boolean =
    that match {
      case node: Node =>
        (this eq node) // optional, but highly recommended sans very specific knowledge about this exact class implementation
        || (node.canEqual(this) // optional only if this class is marked final
          && (hashCode == node.hashCode) // optional, exceptionally execution efficient if hashCode is cached, at an obvious space inefficiency tradeoff
          && ((name == node.name)
            && (leftName == node.leftName)
            && (rightName == node.rightName)))
      case _ =>
        false
    }

  // Intentionally avoiding the call to super.hashCode because no ancestor has overridden hashCode (see note 7 below)
  override def hashCode(): Int =
    31 * (31 * ( name.##) + leftName.##) + rightName.##

object Node:
  val unknown: Node = Node("", "", "")

case class Network(nodes: Map[String, Node], directions: List[Direction]):
  private val loopSize = directions.size

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

  private def betterWalk(current: Node, predicate: Node => Boolean): List[BigInt] =
    @tailrec
    def findAll(
        current: Node,
        directions: Iterator[Direction],
        steps: BigInt,
        result: List[BigInt],
        seen: Set[(Node, BigInt)]
    ): List[BigInt] =
      val next = directions.next()
      if seen.contains((current, steps % loopSize)) then
        result
      else
        val res = if predicate(current) then result :+ steps else result
        next match
          case Direction.Left  => findAll(current.left, directions, steps + 1, res, seen + ((current, steps % loopSize)))
          case Direction.Right => findAll(current.right, directions, steps + 1, res, seen + ((current, steps % loopSize)))
          case _               => throw RuntimeException("Not possible")
    findAll(current, Iterator.continually(directions).flatten, BigInt(0), Nil, Set.empty)

  def run: BigInt = walk(nodes("AAA"), _.name == "ZZZ")
  def ghosts(simple: Boolean): BigInt =
    val starts: List[Node] = nodes.filter(_._1.endsWith("A")).values.toList
    val findFirst = starts.map(n => walk(n, _.name.endsWith("Z")))
    if simple then {
      lcm(findFirst)
    }
    else
      val findAll            = starts.map(n => betterWalk(n, _.name.endsWith("Z")))
      def combinationList[T](ls: List[List[T]]): List[List[T]] = ls match
        case Nil => Nil :: Nil
        case head :: tail =>
          val rec = combinationList[T](tail)
          rec.flatMap(r => head.map(t => t :: r))
      combinationList(findAll).map(lcm).min
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
