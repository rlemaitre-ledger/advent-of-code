package adventofcode.aoc2022.day16

import adventofcode.utils.Edge

final case class Tunnel(from: Valve, to: Valve, cost: Int)
    extends Edge[Valve, Int]
