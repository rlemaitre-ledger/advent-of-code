package adventofcode.aoc2022.day23

import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Coordinates.*

enum Proposition(val lookAt: List[Coordinates], val mainDirection: Coordinates) {
  case North extends Proposition(List(northEast, north, northWest), north)
  case East  extends Proposition(List(northEast, east, southEast), east)
  case South extends Proposition(List(southEast, south, southWest), south)
  case West  extends Proposition(List(northWest, west, southWest), west)
  private def allEmpty(field: Map[Coordinates, Occupation], places: Seq[Coordinates]): Boolean =
    !places.exists(c =>
      field
        .get(c)
        .exists(o => {
          o match
            case Occupation.Elf => true
            case _              => false
        })
    )
  def noElf(field: Map[Coordinates, Occupation], coordinates: Coordinates): Boolean =
    allEmpty(field, lookAt.map(_ + coordinates))
  def goTo(coordinates: Coordinates): Coordinates = coordinates + mainDirection
}
