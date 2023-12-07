package adventofcode.aoc2023.day07

import Combination.*
import adventofcode.Problem
import adventofcode.aoc2023.day07.Combination.FourOfAKind
import adventofcode.inputLines
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import scala.collection.immutable.TreeSet

final case class Day07(input: List[String]) extends Problem[List[String], Int, Int](2023, 7, "Camel Cards"):
  override def part1: Int =
    Hand
      .parseAll(input, false)
      .zipWithIndex
      .map: (hand, rank) =>
        hand.bid * (rank + 1)
      .sum
  override def part2: Int =
    Hand
      .parseAll(input, true)
      .zipWithIndex
      .map: (hand, rank) =>
        hand.bid * (rank + 1)
      .sum

object Day07:
  val instance: Day07 = Day07(inputLines("2023/day07.txt"))

enum Card(val name: Char, val rankWithJoker: Int):
  case Two   extends Card('2', 1)
  case Three extends Card('3', 2)
  case Four  extends Card('4', 3)
  case Five  extends Card('5', 4)
  case Six   extends Card('6', 5)
  case Seven extends Card('7', 6)
  case Eight extends Card('8', 7)
  case Nine  extends Card('9', 8)
  case Ten   extends Card('T', 9)
  case Jack  extends Card('J', 0)
  case Queen extends Card('Q', 10)
  case King  extends Card('K', 11)
  case Ace   extends Card('A', 12)
object Card:
  def fromChar(c: Char): Card = Card.values.find(_.name == c).get

enum Combination:
  case HighCard
  case OnePair
  case TwoPairs
  case ThreeOfAKind
  case FullHouse
  case FourOfAKind
  case FiveOfAKind

extension (c: Combination)
  def valid(countByCard: Map[Card, Int]): Boolean = c match
    case HighCard     => true
    case OnePair      => countByCard.count(_._2 == 2) == 1
    case TwoPairs     => countByCard.count(_._2 == 2) == 2
    case ThreeOfAKind => countByCard.count(_._2 == 3) == 1
    case FullHouse    => countByCard.count(_._2 == 3) == 1 && countByCard.count(_._2 == 2) == 1
    case FourOfAKind  => countByCard.count(_._2 == 4) == 1
    case FiveOfAKind  => countByCard.size == 1
  def plusJokers(count: Int): Combination = c match
    case HighCard =>
      count match
        case 1 => OnePair
        case 2 => ThreeOfAKind
        case 3 => FourOfAKind
        case _ => FiveOfAKind
    case OnePair =>
      count match
        case 1 => ThreeOfAKind
        case 2 => FourOfAKind
        case _ => FiveOfAKind
    case TwoPairs => FullHouse
    case ThreeOfAKind =>
      count match
        case 1 => FourOfAKind
        case 2 => FiveOfAKind
    case FullHouse   => FullHouse
    case FourOfAKind => FiveOfAKind
    case FiveOfAKind => FiveOfAKind

case class Hand(str: String, bid: Bid, withJoker: Boolean) extends Ordered[Hand]:
  val cards: List[Card] =
    List(
      Card.fromChar(str.charAt(0)),
      Card.fromChar(str.charAt(1)),
      Card.fromChar(str.charAt(2)),
      Card.fromChar(str.charAt(3)),
      Card.fromChar(str.charAt(4))
    )
  private val countByCard: Map[Card, Int] = cards.groupBy(identity).view.mapValues(_.size).toMap
  private val cardScore: List[Int]        = cards.map(c => if withJoker then c.rankWithJoker else c.ordinal)
  private val jokerCount: Int             = cards.count(_ == Card.Jack)
  private val withoutJokers: Seq[Card]    = cards.filterNot(_ == Card.Jack)

  private val combination: Combination =
    if withJoker then
      val cardCount = withoutJokers.groupBy(identity).view.mapValues(_.size).toMap
      val combination = Combination.values
        .findLast(_.valid(cardCount))
        .getOrElse(Combination.HighCard)
      if jokerCount == 0 then combination else combination.plusJokers(jokerCount)
    else
      Combination.values
        .findLast(_.valid(this.countByCard))
        .getOrElse(Combination.HighCard)

  override def compare(other: Hand): Int =
    if combination == other.combination then cardScore.compare(other.cardScore)
    else combination.ordinal.compareTo(other.combination.ordinal)
object Hand:
  def parseAll(lines: List[String], withJoker: Boolean): List[Hand] =
    lines.foldLeft(List.empty[Hand])(_ :+ parse(_, withJoker)).sorted
  private def parse(line: String, withJoker: Boolean): Hand =
    val parts = line.split(' ')
    val hand  = Hand(parts.head, Bid(parts.last.toInt.refine), withJoker)
    hand

opaque type Bid <: Int = Int :| Positive
object Bid extends RefinedTypeOps[Int, Positive, Bid]
