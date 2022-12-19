package adventofcode.aoc2022.day16

import adventofcode.utils.Edge

final case class Tunnel(override val from: Valve, override val to: Valve, override val cost: Int)
    extends Edge[Valve, Int](from, to, cost)
