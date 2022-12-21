package adventofcode.aoc2022.day21

import adventofcode.Problem
import adventofcode.inputLines
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.RegexParsers

final case class Day21(input: Map[String, Monkey])
    extends Problem[Map[String, Monkey], BigDecimal, BigDecimal](2022, 21, "Monkey Math") {
  override def part1: BigDecimal = input("root").value
  override def part2: BigDecimal = findValue(input("root").asInstanceOf[OperationMonkey], "humn")
  def findValue(root: OperationMonkey, me: String): BigDecimal =
    val isInFirst    = root.first.contains(me)
    val valueToMatch = if isInFirst then root.second.value else root.first.value
    if isInFirst then root.first.myValue(me, valueToMatch) else root.second.myValue(me, valueToMatch)
}
object Day21 {
  val instance: Day21 = Day21(Monkey.parse(inputLines("2022/day21.txt")))
}
