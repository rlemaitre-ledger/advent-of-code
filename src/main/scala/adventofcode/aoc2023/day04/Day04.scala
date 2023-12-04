package adventofcode.aoc2023.day04
import adventofcode.Problem
import adventofcode.aoc2023.day04.Day04.Cards
import adventofcode.inputLines

final case class Day04(input: List[Card]) extends Problem[List[Card], Long, Long](2023, 4, "Scratchcards"):
  override def part1: Long = input.map(_.score).sum
  override def part2: Long =
    input.foldLeft(Cards(rest = input, seen = Nil, copiesLeft = input.map(_ => 1)))(_ add _).seen.size

object Day04:
  val instance: Day04 = Day04(inputLines("2023/day04.txt").map(Card.parse))

  final case class Cards(seen: List[Card], rest: List[Card], copiesLeft: List[Int]):
    def add(card: Card): Cards =
      val number = copiesLeft.head
      val copies = List.fill(number)(card)
      val value1 = seen ++ copies
      val rest1  = if rest.nonEmpty then rest.tail else Nil
      val left =
        if copiesLeft.isEmpty then Nil
        else
          copiesLeft.tail.zipWithIndex.map:
            case (value, index) => if index < card.winCount then value + number else value
      copy(
        seen = value1,
        rest = rest1,
        copiesLeft = left
      )
  object Cards:
    val empty: Cards = Cards(List.empty, List.empty, List.empty)
