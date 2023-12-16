package adventofcode.aoc2023.day15
import adventofcode.Problem
import adventofcode.aoc2023.day15.Day15.hash
import adventofcode.inputLines

final case class Day15(input: List[String]) extends Problem[List[String], Int, Int](2023, 15, "Lens Library"):
  override def part1: Int = input.head.split(",").map(hash).sum
  override def part2: Int =
    input.head
      .split(",")
      .foldLeft(Map.empty[Int, Box]): (acc, instruction) =>
        instruction. split("=") match
          case Array(label, number) =>
            val n = hash(label)
            acc.updatedWith(n):
              case None => Some(Box.empty(n).replace(label, number.toInt))
              case Some(box) => Some(box.replace(label, number.toInt))
          case _ =>
            val label = instruction.dropRight(1)
            val n = hash(label)
            acc.updatedWith(n):
              case None => Some(Box.empty(n))
              case Some(box) => Some(box.remove(label))
      .values
      .map(_.focusPower)
      .sum

object Day15:
  val instance: Day15 = Day15(inputLines("2023/day15.txt"))

  val factor = 17
  val modulo = 256
  def hash(str: String): Int = str.foldLeft(0)((acc, c) => ((acc + c.toInt) * factor) % modulo)

final case class Box(number: Int, lenses: Vector[(String, Int)]):
  def replace(label: String, n: Int): Box =
    println(s"Box $number: Adding " + label + " " + n)
    lenses.indexWhere(_._1 == label) match
      case -1 => copy(lenses = lenses :+ (label -> n))
      case i: Int => copy(lenses = lenses.updated(i, label -> n))
  def remove(label: String): Box =
    println(s"Box $number: Removing $label")
    lenses.indexWhere(_._1 == label) match
      case -1 => this
      case i: Int => copy(lenses = lenses.patch(i, Nil, 1))
  def focusPower: Int = lenses.zipWithIndex.foldLeft(0):
    case (acc, ((label, n), i)) =>
      val power = acc + ((number + 1) * (i + 1) * n)
      println(s"$label: $power ($number, ${i + 1}, $n)")
      power
object Box:
  def empty(n: Int): Box = Box(n, Vector.empty)
