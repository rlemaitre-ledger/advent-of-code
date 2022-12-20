package adventofcode.aoc2022.day20

import Day20.*
import adventofcode.*
final case class Day20(input: Seq[String])
    extends Problem[Seq[String], Long, Long](2022, 20, "Grove Positioning System") {
  override def part1: Long =
    EncryptedFile.parse(input).rotate().coordinates.sum

  override def part2: Long =
    EncryptedFile.parse(input, decryptionKey).rotate(10).coordinates.sum

}
object Day20 {
  val instance: Day20     = Day20(inputLines("2022/day20.txt"))
  val decryptionKey: Long = 811589153L
}
