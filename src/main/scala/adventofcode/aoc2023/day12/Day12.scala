package adventofcode.aoc2023.day12

import adventofcode.*
import adventofcode.aoc2023.day12.Spring.*
import adventofcode.inputLines
import scala.annotation.targetName
import scala.collection.immutable.::

final case class Day12(input: List[String]) extends Problem[List[String], Int, Int](2023, 12, "Hot Springs"):
  override def part1: Int =
    input.map(Line.parse).map(Line.combinations).sum
  override def part2: Int =
    input.map(Line.parse).map(_ * 5).map(Line.combinations).sum

object Day12:
  val instance: Day12 = Day12(inputLines("2023/day12.txt"))

enum Spring:
  case Damaged(size: Int)
  case Operational
  case Unknown
object Spring:
  def damaged(n: Int): Spring = Damaged(n)
  def compress(springs: List[Spring]): List[Spring] =
    val compressed = springs.foldLeft(List.empty[Spring]):
      case (head :: tail, Unknown)            => Unknown :: head :: tail
      case (existing, Unknown)                => Unknown :: existing
      case (Nil, Operational)                 => List(Operational)
      case (Operational :: tail, Operational) => Operational :: tail
      case (head :: tail, Operational)        => Operational :: head :: tail
      case (Nil, Damaged(n))                  => List(Damaged(n))
      case (Damaged(n) :: tail, Damaged(m))   => Damaged(n + m) :: tail
      case (head :: tail, Damaged(n))         => Damaged(n) :: head :: tail
    compressed.dropWhile(_ == Operational).reverse.dropWhile(_ == Operational)
  def resolve(groups: List[Spring]): List[List[Spring]] =
    if groups.contains(Unknown) then
      val i                    = groups.indexOf(Unknown)
      val (before, _ :: after) = groups.splitAt(i): @unchecked
      resolve(Operational :: after).map(before ++ _) ++
        resolve(Damaged(1) :: after).map(before ++ _)
    else List(groups)
  def display(springs: List[Spring]): String =
    springs
      .map:
        case Damaged(n)  => "#" * n
        case Operational => "."
        case Unknown     => "?"
      .mkString
final case class Line(str: String, groupSizes: List[Int]):
  @targetName("multiply")
  def *(n: Int): Line = Line(List.fill(n)(str).mkString("?"), List.fill(n)(groupSizes).flatten)
  def candidateGroups: List[Spring] =
    str
      .foldLeft(List.empty[Spring]):
        case (Nil, '#')                 => List(Damaged(1))
        case (Damaged(n) :: tail, '#')  => Damaged(n + 1) :: tail
        case (head :: tail, '#')        => Damaged(1) :: head :: tail
        case (Nil, '.')                 => List(Operational)
        case (Operational :: tail, '.') => Operational :: tail
        case (head :: tail, '.')        => Operational :: head :: tail
        case (acc, '?')                 => Unknown :: acc
        case _                          => throw IllegalStateException("💥 Invalid character")
      .reverse
  def damaged: List[Spring] = groupSizes.map(Spring.damaged).intersperse(Operational)

object Line:

  def parse(line: String): Line =
    val conditionRecord :: groups :: Nil = line.split(" ").toList: @unchecked
    val groupSizes                       = groups.split(',').map(_.toInt).toList
    Line(conditionRecord, groupSizes)

  def combinations(line: Line): Int =
    Spring.resolve(line.candidateGroups).map(Spring.compress).count(_ == line.damaged)

  private def compress(s: String) = s.replaceAll("\\.{2,}", ".").stripAll(".")
  private def removeGroupsSimple(s: String, groups: List[Int]) =
    val (preprocessedString: String, remainingGroupSizes: List[Int], positionInGroup: Int, endedInGroup: Boolean, _) =
      s.foldLeft(("", groups, 0, false, false)):
        case ((accString, accGroupSizes, currentSize, inGroup, afterWildcard), char) =>
          if afterWildcard then (accString + char, accGroupSizes, currentSize, inGroup, true)
          else
            char match
              case '#' if inGroup => (accString, accGroupSizes, currentSize + 1, true, afterWildcard)
              case '#'            => (accString, accGroupSizes, 1, true, afterWildcard)
              case '.' if inGroup =>
                if accGroupSizes.headOption.contains(currentSize) then
                  (accString + ".", accGroupSizes.tail, 0, false, afterWildcard)
                else (accString + "#" * currentSize + ".", accGroupSizes, 0, false, afterWildcard)
              case '.' => (accString + ".", accGroupSizes, currentSize, false, afterWildcard)
              case '?' => (accString + "?", accGroupSizes, currentSize, false, true)
    if remainingGroupSizes.headOption.contains(positionInGroup) then
      (preprocessedString.stripAll("."), remainingGroupSizes.tail)
    else ((preprocessedString + "#" * positionInGroup).stripAll("."), remainingGroupSizes)
  def simplify(line: Line): Line =
    val (s1, g1) = removeGroupsSimple(compress(line.str), line.groupSizes)
    val (s2, g2) = removeGroupsSimple(s1.reverse, g1.reverse)
    Line(s2.reverse, g2.reverse)
