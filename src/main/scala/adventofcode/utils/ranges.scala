package adventofcode.utils.range

import cats.Monoid
import cats.syntax.all.*
import scala.annotation.targetName
import scala.collection.immutable.NumericRange
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordered.orderingToOrdered
import scala.util.matching.Regex

final case class Range[T](from: T, to: T)(using num: Integral[T]):
  require(from <= to, s"invalid range: $from > $to")

  def grouped(size: Int): List[Range[T]] =
    toIterable.grouped(size).map(i => Range(i.head, i.last)).toList

  def toIterable: Iterable[T] = NumericRange.Inclusive[T](from, to, num.one)

  def length: T = to - from

  def toMultiRange: MultiRange[T] = MultiRange.of(this)

  def readable: String = s"[$from, $to]"

  def overlaps(other: Range[T]): Boolean = this.to >= other.from && other.to >= this.from

  def contiguous(other: Range[T]): Boolean = this.to + num.one == other.from || this.from == other.to + num.one

  def union(other: Range[T]): Range[T] =
    require(
      overlaps(other) || contiguous(other),
      show"Can only be called on overlapping or contiguous ranges"
    )
    Range(num.min(this.from, other.from), num.max(this.to, other.to))

  def intersect(other: Range[T]): Range[T] =
    require(
      overlaps(other) || contiguous(other),
      show"Can only be called on overlapping or contiguous ranges"
    )
    Range(num.max(this.from, other.from), num.min(this.to, other.to))

  @targetName("plus")
  def +(other: Range[T]): MultiRange[T] =
    if overlaps(other) || contiguous(other) then MultiRange.of(this union other)
    else MultiRange.of(this, other)

  @targetName("minus")
  def -(other: Range[T]): MultiRange[T] =
    if overlaps(other) then
      val newRanges = List(
        if (other.from > this.from) Some(Range(this.from, other.from - num.one)) else None,
        if (other.to < this.to) Some(Range(other.to + num.one, this.to)) else None
      ).flatten
      MultiRange(newRanges)
    else MultiRange.of(this)

object Range:
  def single[T](h: T)(using Integral[T]): Range[T] = Range(h, h)

  val pattern: Regex = """[\[(] *(\d*), *(\d*) *[)\]]""".r

  def parse[T](str: String)(using num: Integral[T]): Either[String, Range[T]] =
    val errorMsg = s"Invalid format $str must have the ${pattern.regex} format"
    val error    = Left(errorMsg)
    pattern.findFirstMatchIn(str.trim) match
      case Some(regexMatch) =>
        for
          lowerBound <-
            if (regexMatch.group(1).isEmpty) error
            else Either.fromOption(num.parseString(regexMatch.group(1)), errorMsg)
          from <- if (str.startsWith("[")) lowerBound.asRight else (lowerBound + num.one).asRight
          upperBound <-
            if (regexMatch.group(2).isEmpty) error
            else Either.fromOption(num.parseString(regexMatch.group(2)), errorMsg)
          to <- if (str.endsWith("]")) upperBound.asRight else (upperBound - num.one).asRight
        yield Range(from, to)
      case None => error

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
final case class MultiRange[T] private (private val ranges: List[Range[T]])(using num: Integral[T])
    extends Iterable[Range[T]]:

  override def iterator: Iterator[Range[T]] = ranges.iterator

  override def toList: List[Range[T]] = ranges

  def firstTop: Option[T] = ranges.headOption.map(_.to)

  def readable: String = ranges.map(_.readable).mkString("{", ", ", "}")

  @targetName("plusRange")
  def +(range: Range[T]): MultiRange[T] =
    val (before, endAfter)   = ranges.span(_.to + num.one < range.from)
    val (overlapping, after) = endAfter.span(_.from <= range.to + num.one)
    val newRange             = overlapping.foldLeft(range)(_ union _)
    val beginning = before.lastOption match
      case Some(rangeBefore) if rangeBefore.contiguous(newRange) => before.init ++ List(rangeBefore union newRange)
      case Some(rangeBefore)                                     => before :+ newRange
      case _                                                     => List(newRange)
    MultiRange(beginning ++ after)

  @targetName("plusMultiRange")
  def +(other: MultiRange[T]): MultiRange[T] = other.ranges.foldLeft(this)(_ + _)

  @targetName("minusRange")
  def -(range: Range[T]): MultiRange[T] = MultiRange(ranges.flatMap(_ - range))

  @targetName("minusMultiRange")
  def -(other: MultiRange[T]): MultiRange[T] = other.ranges.foldLeft(this)(_ - _)

  def holes: MultiRange[T] =
    if ranges.nonEmpty then Range(ranges.map(_.from).min, ranges.map(_.to).max).toMultiRange - this
    else MultiRange.empty

object MultiRange:
  def empty[T: Integral]: MultiRange[T] = MultiRange(List.empty)

  def of[T: Integral](range: Range[T], others: Range[T]*): MultiRange[T] = MultiRange(range +: others)

  def apply[T: Integral](ranges: Iterable[Range[T]]): MultiRange[T] = ranges.foldLeft(MultiRange.empty)(_ + _)

  def concat[T: Integral](ranges: Iterable[MultiRange[T]]): MultiRange[T] = ranges.reduce(_ + _)

  def parse[T: Integral](str: String): Either[String, MultiRange[T]] =
    val trimmed = str.trim
    if trimmed == "{}" then Right(MultiRange.empty)
    else if trimmed.startsWith("{") && trimmed.endsWith("}") then
      val rangesStr = trimmed.drop(1).dropRight(1).trim
      val ranges    = Range.pattern.findAllIn(rangesStr).map(Range.parse).toList
      if ranges.forall(_.isRight) then
        val value: List[Range[T]] = ranges.map(_.getOrElse(throw new Exception("This should never happen")))
        Right(MultiRange.of(value.head, value.tail: _*))
      else Left(s"Invalid format: At least one range in $trimmed cannot be parsed")
    else Left(s"Invalid format: $trimmed should start by { and end by }")

  given [T: Integral]: Monoid[MultiRange[T]] with
    def empty: MultiRange[T] = MultiRange.empty

    def combine(x: MultiRange[T], y: MultiRange[T]): MultiRange[T] = x + y
