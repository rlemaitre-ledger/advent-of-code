package adventofcode

import adventofcode.Mode.Part1
import adventofcode.Mode.Part2
import scala.io.Source

trait Problem[I, T1, T2](val name: String):
  def input: I
  def part1: T1
  def part2: T2
  def run(mode: Mode): T1 | T2 = mode match
    case Part1 => part1
    case Part2 => part2
  def run: (T1, T2) =
    println(banner)
    println("Running Part 1")
    val p1 = part1
    println(s"\t$p1")
    println("Running Part 2")
    val p2 = part2
    println(s"\t$p2")
    (p1, p2)
  private val topBanner: String    = "╭─" + " ".repeat(name.length) + "─╮"
  private val bottomBanner: String = "╰─" + " ".repeat(name.length) + "─╯"
  val banner: String =
    s"""
       |$topBanner
       |  $name
       |$bottomBanner
       |""".stripMargin
