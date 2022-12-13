package adventofcode

object Day06 extends AdventOfCodeBase[Int, Int]("day06.txt"):
  def startOfPacket(signal: String, length: Int): Int =
    (0 until signal.length)
      .map(Buffer(signal, _, length))
      .find(_.isValid)
      .map(_.end)
      .getOrElse(-1)
  override def part1(lines: List[String]): Int = startOfPacket(lines.head, Buffer.packetLength)
  override def part2(lines: List[String]): Int = startOfPacket(lines.head, Buffer.messageLength)
  final case class Buffer(source: String, start: Int, length: Int):
    val end: Int         = start + length // end bound is exclusive
    val isValid: Boolean = end <= source.length && source.substring(start, end).toSet.size == length
  object Buffer:
    val packetLength: Int  = 4
    val messageLength: Int = 14
