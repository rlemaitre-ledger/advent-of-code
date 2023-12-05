package adventofcode.aoc2023.day05
import cats.Monoid
import cats.syntax.all.*

import scala.annotation.targetName
import scala.util.matching.Regex

final case class Range(from: Long, to: Long):
  require(from <= to, s"invalid range: $from > $to")

  def grouped(size: Int): List[Range] =
    toIterable.grouped(size).map(i => Range(i.head, i.last)).toList

  def toIterable: Iterable[Long] = from to to

  def length: Long = to - from

  def toMultiRange: MultiRange = MultiRange.of(this)

  def readable: String = s"[$from, $to]"

  def overlaps(other: Range): Boolean = this.to >= other.from && other.to >= this.from

  def contiguous(other: Range): Boolean = this.to + 1 == other.from || this.from == other.to + 1

  def union(other: Range): Range =
    require(
      overlaps(other) || contiguous(other),
      show"Can only be called on overlapping or contiguous ranges"
    )
    Range(this.from min other.from, this.to max other.to)

  def intersect(other: Range): Range =
    require(
      overlaps(other) || contiguous(other),
      show"Can only be called on overlapping or contiguous ranges"
    )
    Range(this.from max other.from, this.to min other.to)

  @targetName("plus")
  def +(other: Range): MultiRange =
    if overlaps(other) || contiguous(other) then MultiRange.of(this union other)
    else MultiRange.of(this, other)

  @targetName("minus")
  def -(other: Range): MultiRange =
    if overlaps(other) then
      val newRanges = List(
        if (other.from > this.from) Some(Range(this.from, other.from - 1)) else None,
        if (other.to < this.to) Some(Range(other.to + 1, this.to)) else None
      ).flatten
      MultiRange(newRanges)
    else MultiRange.of(this)

object Range:
  def single(h: Long): Range = Range(h, h)

  val pattern: Regex = """[\[(] *(\d*), *(\d*) *[)\]]""".r

  def parse(str: String): Either[String, Range] = {
    pattern.findFirstMatchIn(str.trim) match
      case Some(regexMatch) =>
        val lowerBound = if (regexMatch.group(1).isEmpty) 0L else regexMatch.group(1).toLong
        val from = if (str.startsWith("[")) lowerBound else lowerBound + 1
        val upperBound = if (regexMatch.group(2).isEmpty) Long.MaxValue else regexMatch.group(2).toLong
        val to = if (str.endsWith("]")) upperBound else upperBound - 1
        Right(Range(from, to))
      case None => Left(s"Invalid format $str must have the ${pattern.regex} format")
  }

given math.Ordering[Long] = (a: Long, b: Long) => a.compare(b)

/** Represents a collection of non-overlapping ranges, sorted by their `from` field.
 *
 * @constructor
 * Creates a new `MultiRange` with a list of non-overlapping ranges. The constructor is private; use
 * `MultiRange.empty` or `MultiRange.of` to create instances.
 * @param ranges
 * The list of non-overlapping ranges that make up the `MultiRange`.
 *
 * ==Examples==
 * {{{
 * val mr1 = MultiRange.of(Range(1, 5), Range(6, 10))
 * val mr2 = MultiRange.of(Range(11, 15))
 * val combined = mr1 + mr2 // MultiRange containing List(Range(1, 5), Range(6, 10), Range(11, 15))
 * }}}
 * @note
 * The ranges in `MultiRange` are always non-overlapping and sorted.
 *
 * ==Factory Methods==
 *   - `MultiRange.empty`: Creates an empty `MultiRange`.
 *   - `MultiRange.of`: Creates a `MultiRange` from one or more `Range` instances.
 *
 * ==Operations==
 *   - `+`: Adds a `Range` or another `MultiRange` to this `MultiRange`.
 *   - `-`: Removes a `Range` or another `MultiRange` from this `MultiRange`.
 */
final case class MultiRange private(private val ranges: List[Range]) extends Iterable[Range]:

  override def iterator: Iterator[Range] = ranges.iterator

  override def toList: List[Range] = ranges

  def firstTop: Option[Long] = ranges.headOption.map(_.to)

  def readable: String = ranges.map(_.readable).mkString("{", ", ", "}")

  @targetName("plusRange")
  def +(range: Range): MultiRange =
    val (before, endAfter) = ranges.span(_.to + 1 < range.from)
    val (overlapping, after) = endAfter.span(_.from <= range.to + 1)
    val newRange = overlapping.foldLeft(range)(_ union _)
    val beginning = before.lastOption match
      case Some(rangeBefore) if rangeBefore.contiguous(newRange) => before.init ++ List(rangeBefore union newRange)
      case Some(rangeBefore) => before :+ newRange
      case _ => List(newRange)
    MultiRange(beginning ++ after)

  @targetName("plusMultiRange")
  def +(other: MultiRange): MultiRange = other.ranges.foldLeft(this)(_ + _)

  @targetName("minusRange")
  def -(range: Range): MultiRange = MultiRange(ranges.flatMap(_ - range))

  @targetName("minusMultiRange")
  def -(other: MultiRange): MultiRange = other.ranges.foldLeft(this)(_ - _)

  def holes: MultiRange =
    if ranges.nonEmpty then Range(ranges.map(_.from).min, ranges.map(_.to).max).toMultiRange - this
    else MultiRange.empty

object MultiRange:
  val empty: MultiRange = MultiRange(List.empty)

  def of(range: Range, others: Range*): MultiRange = MultiRange(range +: others)

  def apply(ranges: Iterable[Range]): MultiRange = ranges.foldLeft(MultiRange.empty)(_ + _)

  def concat(ranges: Iterable[MultiRange]): MultiRange = ranges.reduce(_ + _)

  def parse(str: String): Either[String, MultiRange] =
    val trimmed = str.trim
    if trimmed == "{}" then Right(MultiRange.empty)
    else if trimmed.startsWith("{") && trimmed.endsWith("}") then
      val rangesStr = trimmed.drop(1).dropRight(1).trim
      val ranges = Range.pattern.findAllIn(rangesStr).map(Range.parse).toList
      if ranges.forall(_.isRight) then
        val value: List[Range] = ranges.map(_.getOrElse(throw new Exception("This should never happen")))
        Right(MultiRange.of(value.head, value.tail: _*))
      else Left(s"Invalid format: At least one range in $trimmed cannot be parsed")
    else Left(s"Invalid format: $trimmed should start by { and end by }")

  given Monoid[MultiRange] with {
    def empty: MultiRange = MultiRange.empty

    def combine(x: MultiRange, y: MultiRange): MultiRange = x + y
  }
