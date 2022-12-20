package adventofcode.aoc2022.day19

import scala.annotation.targetName

final case class Resources(ore: Int, clay: Int, obsidian: Int, geode: Int) {
  @targetName("plus")
  def +(r: Resources): Resources = Resources(ore + r.ore, clay + r.clay, obsidian + r.obsidian, geode + r.geode)
  @targetName("minus")
  def -(r: Resources): Resources = Resources(ore - r.ore, clay - r.clay, obsidian - r.obsidian, geode - r.geode)
  @targetName("lowerOrEquals")
  def <=(r: Resources): Boolean = ore <= r.ore && clay <= r.clay && obsidian <= r.obsidian && geode <= r.geode
  @targetName("greaterOrEquals")
  def >=(r: Resources): Boolean = ore >= r.ore && clay >= r.clay && obsidian >= r.obsidian && geode >= r.geode
  @targetName("times")
  def*(n: Int): Resources = Resources(ore * n, clay * n, obsidian * n, geode * n)
}
object Resources {
  val zero: Resources        = Resources(0, 0, 0, 0)
  val oreBot: Resources      = Resources(1, 0, 0, 0)
  val clayBot: Resources     = Resources(0, 1, 0, 0)
  val obsidianBot: Resources = Resources(0, 0, 1, 0)
  val geodeBot: Resources    = Resources(0, 0, 0, 1)
}
