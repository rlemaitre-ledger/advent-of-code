package adventofcode.aoc2023.day13
import adventofcode.Problem
import adventofcode.inputLines
import scala.annotation.tailrec

final case class Day13(input: List[String]) extends Problem[List[String], Int, Int](2023, 13, "Point of Incidence"):
  private val blocks: List[List[String]] =
    @tailrec
    def impl(list: List[String], result: List[List[String]]): List[List[String]] =
      list match
        case Nil                                          => result
        case head :: tail if head.isEmpty || head.isBlank => impl(tail, result :+ Nil)
        case head :: tail                                 => impl(tail, result.init :+ (result.last :+ head))
    impl(input, List(Nil))
  override def part1: Int = blocks.map(Pattern.parse).map(_.axis(0)).sum
  override def part2: Int = blocks.map(Pattern.parse).map(_.axis(1)).sum

object Day13:
  val instance: Day13 = Day13(inputLines("2023/day13.txt"))

final case class Pattern(content: List[String], nbCols: Int, nbRows: Int):
  val cols: List[String] = content.transpose.map(_.mkString)
  private def reflectionAxis(set: List[String], count: Int, nbSubstitution: Int): Option[Int] =
    val value: List[Map[Int, Int]] = set
      .map: s =>
        val map = (
          s.tails.map(suffix => suffix -> (s.length - suffix.length + (suffix.length / 2))) ++
            s.inits.map(prefix => prefix -> (prefix.length / 2))
        )
          .filter((s, _) => s.length >= 2 && s.length % 2 == 0)
          .map: (suffix, axis) =>
            val distance =
              Pattern.substitutionDistance(suffix.take(suffix.length / 2), suffix.takeRight(suffix.length / 2).reverse)
            axis -> distance
          .toMap
        map
    value
      .foldLeft(Map.empty[Int, Int]): (acc, distanceByAxis) =>
        distanceByAxis
          .foldLeft(acc):
            case (acc, (axis, distance)) =>
              acc.updatedWith(axis)(_.map(_ + distance).orElse(Some(distance)))
      .find(_._2 == nbSubstitution)
      .map(_._1)

  def axis(nbDiff: Int): Int = {
    reflectionAxis(content, nbCols, nbDiff).orElse(reflectionAxis(cols, nbRows, nbDiff).map(_ * 100)).getOrElse(0)
  }

object Pattern:
  def parse(input: List[String]): Pattern =
    val nbCols = input.head.length
    val nbRows = input.length
    Pattern(input, nbCols, nbRows)
  def substitutionDistance[T](a: Iterable[T], b: Iterable[T]): Int = a.zip(b).count((a, b) => a != b)
