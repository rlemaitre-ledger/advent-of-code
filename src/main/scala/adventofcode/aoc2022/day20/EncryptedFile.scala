package adventofcode.aoc2022.day20

import EncryptedFile.*
final case class EncryptedFile(nodes: Seq[Node]) {
  val start: Node = nodes.find(_.value == 0).get
  def rotate(times: Int = 1): EncryptedFile = {
    for _ <- 1 to times do {
      for node <- nodes do {
        val remainder = (node.value % (nodes.size - 1)).toInt
        val move      = if remainder >= 0 then remainder else remainder + nodes.size - 1
        for _ <- 1 to move do {
          val (previous, current, next, nextNext) = (node.prev, node, node.next, node.next.next)
          previous.next = next
          current.prev = next
          current.next = nextNext
          next.prev = previous
          next.next = current
          nextNext.prev = current
        }
      }
    }
    this
  }

  def valueAt(count: Int): Long = {
    Iterator.iterate(start)(_.next).drop(count).next().value
  }
  def coordinates: List[Long] =
    List(valueAt(1000), valueAt(2000), valueAt(3000))

}
object EncryptedFile {
  case class Node(value: Long, var prev: Node, var next: Node)
  def parse(lines: Seq[String], decryptionKey: Long = 1L): EncryptedFile =
    val nodes = lines.map(n => Node(n.toLong * decryptionKey, null, null))
    nodes.zipWithIndex.foreach { case (node, index) =>
      node.prev = nodes((index - 1 + nodes.size) % nodes.size)
      node.next = nodes((index + 1) % nodes.size)
    }
    EncryptedFile(nodes)

}
