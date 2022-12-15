package adventofcode

import adventofcode.Day06.*

case class Day06(input: String) extends AdventOfCodeBase[String, Int, Int] {
  def startOfPacket(length: Int): Int =
    (0 until input.length)
      .map(Buffer(input, _, length))
      .find(_.isValid)
      .map(_.end)
      .getOrElse(-1)

  override def part1: Int = startOfPacket(Buffer.packetLength)

  override def part2: Int = startOfPacket(Buffer.messageLength)
}
object Day06 {
  val instance: Day06 = Day06(inputLines("day06.txt").head)
  final case class Buffer(source: String, start: Int, length: Int) {
    val end: Int         = start + length // end bound is exclusive
    val isValid: Boolean = end <= source.length && source.substring(start, end).toSet.size == length
  }
  object Buffer {
    val packetLength: Int  = 4
    val messageLength: Int = 14
  }
}
