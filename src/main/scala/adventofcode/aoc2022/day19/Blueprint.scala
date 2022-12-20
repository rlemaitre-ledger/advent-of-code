package adventofcode.aoc2022.day19

final case class Blueprint(index: Int, ore: Resources, clay: Resources, obsidian: Resources, geode: Resources) {
  val maxCosts: Resources =
    Resources(
      ore = List(ore.ore, clay.ore, obsidian.ore, geode.ore).max,
      clay = List(ore.clay, clay.clay, obsidian.clay, geode.clay).max,
      obsidian = List(ore.obsidian, clay.obsidian, obsidian.obsidian, geode.obsidian).max,
      geode = List(ore.geode, clay.geode, obsidian.geode, geode.geode).max
    )
}

object Blueprint {
  def fromString(str: String): Blueprint = str match {
    case s"Blueprint $index: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $obsidianOre ore and $obsidianClay clay. Each geode robot costs $geodeOre ore and $geodeObsidian obsidian." =>
      Blueprint(
        index.toInt,
        Resources(ore.toInt, 0, 0, 0),
        Resources(clay.toInt, 0, 0, 0),
        Resources(obsidianOre.toInt, obsidianClay.toInt, 0, 0),
        Resources(geodeOre.toInt, 0, geodeObsidian.toInt, 0)
      )
  }
  def parse(lines: List[String]): List[Blueprint] = lines.map(fromString)
}
