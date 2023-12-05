package adventofcode.aoc2022.day19

import adventofcode.Problem
import adventofcode.aoc2022.day19.Day19.*
import adventofcode.inputLines
import adventofcode.time
import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.*

final case class Day19(input: List[Blueprint])
    extends Problem[List[Blueprint], Int, Long](2022, 19, "Not Enough Minerals") {
  override def part1: Int =
    input.map { blueprint =>
      val maxGeode = simulate(24, blueprint, Resources.oreBot, Resources.zero, false, false, false)
      blueprint.index * maxGeode
    }.sum
  override def part2: Long = input
    .take(3)
    .map { blueprint =>
      val maxGeode = simulate(32, blueprint, Resources.oreBot, Resources.zero, false, false, false)
      maxGeode.toLong
    }
    .product
  def simulate(
      minute: Int,
      blueprint: Blueprint,
      bots: Resources,
      resources: Resources,
      couldBuildOre: Boolean,
      couldBuildClay: Boolean,
      couldBuildObsidian: Boolean
  ): Int = {
    if (minute == 0) {
      resources.geode
    } else if (blueprint.geode <= resources) {
      simulate(
        minute - 1,
        blueprint,
        bots + Resources.geodeBot,
        resources + bots - blueprint.geode,
        false,
        false,
        false
      )
    } else {
      val canOre         = blueprint.ore <= resources && bots.ore < blueprint.maxCosts.ore
      val canClay        = blueprint.clay <= resources && bots.clay < blueprint.maxCosts.clay
      val canObsidian    = blueprint.obsidian <= resources && bots.obsidian < blueprint.maxCosts.obsidian
      val noConstruction = simulate(minute - 1, blueprint, bots, resources + bots, canOre, canClay, canObsidian)
      val constructOre =
        if (canOre && !couldBuildOre) {
          simulate(
            minute - 1,
            blueprint,
            bots + Resources.oreBot,
            resources + bots - blueprint.ore,
            false,
            false,
            false
          )
        } else 0
      val constructClay =
        if (canClay && !couldBuildClay)
          simulate(
            minute - 1,
            blueprint,
            bots + Resources.clayBot,
            resources + bots - blueprint.clay,
            false,
            false,
            false
          )
        else 0
      val constructObsidian =
        if (canObsidian && !couldBuildObsidian)
          simulate(
            minute - 1,
            blueprint,
            bots + Resources.obsidianBot,
            resources + bots - blueprint.obsidian,
            false,
            false,
            false
          )
        else 0
      noConstruction.max(constructOre).max(constructClay).max(constructObsidian)
    }
  }

}
object Day19 {
  val instance: Day19 = Day19(Blueprint.parse(inputLines("2022/day19.txt")))
}
