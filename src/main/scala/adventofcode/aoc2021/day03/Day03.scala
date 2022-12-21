package adventofcode.aoc2021.day03
import adventofcode.Problem
import adventofcode.inputLines
import scala.annotation.tailrec

final case class Day03(input: List[String]) extends Problem[List[String], Int, Int](2021, 3, "Binary Diagnostic") {
  private val diagnosticReport: List[Array[Int]] = input.map(_.map(_.toString.toInt).toArray)
  private val bitCount: Int                      = input.head.length
  private def oneMostCommon(numbers: List[Array[Int]]): Array[Boolean] =
    val countPerBit: Array[Int] = numbers.foldLeft(Array.fill(bitCount)(0)) { (acc, current) =>
      (acc zip current).map { case (a, b) => a + b }
    }
    val limit = if numbers.length % 2 == 0 then numbers.length / 2 else (numbers.length + 1) / 2
    countPerBit.map(_ >= limit)

  override def part1: Int = {
    val mostCommon     = oneMostCommon(diagnosticReport)
    val epsilonRateBit = mostCommon.map(b => if b then 0 else 1)
    val gammaRateBit   = mostCommon.map(b => if b then 1 else 0)
    toDecimal(epsilonRateBit) * toDecimal(gammaRateBit)
  }
  private def toDecimal(bits: Array[Int]): Int =
    @tailrec
    def step(acc: Int, array: Array[Int]): Int =
      if (array.length == 0) {
        acc
      } else {
        step(acc + (array.head << (array.length - 1)), array.tail)
      }
    step(0, bits)
  override def part2: Int =
    val oxygenRating = findWith(diagnosticReport, 0, true)
    val co2Rating    = findWith(diagnosticReport, 0, false)
    toDecimal(oxygenRating) * toDecimal(co2Rating)
  @tailrec
  private def findWith(numbers: List[Array[Int]], index: Int, most: Boolean): Array[Int] = {
    val mostCommon = oneMostCommon(numbers)
    val filtered   = numbers.filter(bits => if mostCommon(index) == most then bits(index) == 1 else bits(index) == 0)
    if filtered.length == 1 then filtered.head
    else findWith(filtered, index + 1, most)
  }

}
object Day03 {
  val instance: Day03 = Day03(inputLines("2021/day03.txt"))
}
