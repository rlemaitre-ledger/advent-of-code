package adventofcode.aoc2023.day04

final case class Card(id: Int, winning: Set[Int], numbers: Set[Int]):
  val winningNumbers: Set[Int] = winning intersect numbers
  val winCount: Int            = winningNumbers.size
  val score: Long              = if winCount == 0 then 0 else 1 << (winCount - 1)

object Card:
  def parse(line: String): Card =
    val s    = line.split(':')
    val id   = s.head.split(' ').last.toInt
    val ns   = s.last.split('|')
    val wins = ns.head.split(' ').filter(_.nonEmpty).map(_.trim.toInt).toSet
    val nums = ns.last.split(' ').filter(_.nonEmpty).map(_.trim.toInt).toSet
    Card(id, wins, nums)
