package adventofcode.day17

import Day17.*
import adventofcode.*
import adventofcode.Problem

final case class Day17(input: String) extends Problem[String, Int, Long]("Pyroclastic Flow") {

  def simulate: Iterator[State] = {
    val initial = State(input, Grid.empty, 0, 0, 0)
    Iterator.iterate(initial)(_.step)
  }

  override def part1: Int = heightAfter(2022)

  def heightAfter(rocks: Int): Int = simulate.drop(rocks).next().height

  override def part2: Long = {
//    val state            = simulate.dropWhile(_.topLineIsNotFull).next()
    val guess            = 1000
    val height: Seq[Int] = simulate.slice(1, 5 * guess).map(_.height).toSeq
    val delta: Seq[Int]  = height.sliding(2).map(s => s.last - s.head).toSeq
    val end: Int         = delta.size - guess
    val start: Int       = delta.lastIndexOfSlice(delta.takeRight(guess), end - 1)
    val cycleHeight: Int = height(end) - height(start)
    val cycleWidth: Int  = end - start
    val offset: Long     = 1000000000000L - 1 - start
    val quotient: Long   = offset / cycleWidth
    val remainder: Long  = offset % cycleWidth
    (quotient * cycleHeight) + height(start + remainder.toInt)

  }
}

object Day17 {
  val instance: Day17 = Day17(inputLines("day17.txt").head)

}
