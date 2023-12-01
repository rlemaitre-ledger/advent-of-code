package adventofcode.aoc2022.day23

import adventofcode.aoc2022.day23.Field.*
import adventofcode.aoc2022.day23.Occupation.Empty
import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Coordinates.*

final case class Field(
    field: Map[Coordinates, Occupation],
    previousField: Map[Coordinates, Occupation],
    propositions: List[Proposition],
    round: Int
) {
  val elves = field.filter(_._2 match
    case Empty          => false
    case Occupation.Elf => true
  )

  val elvesMoved: Boolean                           = field != previousField
  private def rotatePropositions: List[Proposition] = propositions.tail ++ List(propositions.head)
  def next: Field =
    val wishes = elves
      .map { (c, o) =>
        proposition(c, o) match
          case Some(value) => (c, value)
          case None        => (c, c)
      }
      .filterNot(p => p._1 == p._2)
    val toMove: List[(Coordinates, Coordinates)] =
      wishes.groupBy(_._2).filter(_._2.size == 1).values.foldLeft(List.empty[(Coordinates, Coordinates)])(_ ++ _)
    val newField = toMove
      .foldLeft(field) { case (acc, (from, to)) =>
        val occupation = field(from)
        (acc + (from -> Occupation.Empty)) + (to -> occupation)
      }
    copy(field = newField, previousField = field, propositions = rotatePropositions, round = round + 1)
  def score: Int =
    val minX = elves.keySet.map(_.x).min
    val maxX = elves.keySet.map(_.x).max
    val minY = elves.keySet.map(_.y).min
    val maxY = elves.keySet.map(_.y).max
    (1 + maxX - minX) * (1 + maxY - minY) - elves.size
  private def proposition(coordinates: Coordinates, occupation: Occupation): Option[Coordinates] = {
    if emptyAround(field, coordinates) then None
    else {
      if propositions(0).noElf(field, coordinates) then Some(propositions(0).goTo(coordinates))
      else if propositions(1).noElf(field, coordinates) then Some(propositions(1).goTo(coordinates))
      else if propositions(2).noElf(field, coordinates) then Some(propositions(2).goTo(coordinates))
      else if propositions(3).noElf(field, coordinates) then Some(propositions(3).goTo(coordinates))
      else None
    }
  }
  def show: String =
    val minX = field.keySet.map(_.x).min
    val maxX = field.keySet.map(_.x).max
    val minY = field.keySet.map(_.y).min
    val maxY = field.keySet.map(_.y).max
    (minY to maxY).reverse
      .map(line =>
        (minX to maxX)
          .map(c =>
            field.get(Coordinates(c, line)) match
              case Some(Occupation.Elf) => "#"
              case _                    => "."
          )
          .mkString
      )
      .mkString("\n")
}
object Field {
  def parse(lines: List[String]): Field = {
    val map: Map[Coordinates, Occupation] = lines.zipWithIndex.flatMap { (line, lineIndex) =>
      line.zipWithIndex.map { case (c, columnIndex) =>
        val occupation = if c == '#' then Occupation.Elf else Occupation.Empty
        Coordinates(columnIndex, (lines.length - 1) - lineIndex) -> occupation
      }
    }.toMap
    Field(map, Map.empty, List(Proposition.North, Proposition.South, Proposition.West, Proposition.East), 0)
  }

  private def allEmpty(field: Map[Coordinates, Occupation], places: Coordinates*): Boolean =
    !places.exists(c =>
      field
        .get(c)
        .exists(o => {
          o match
            case Occupation.Elf => true
            case _              => false
        })
    )

  def emptyAround(field: Map[Coordinates, Occupation], coordinates: Coordinates): Boolean =
    allEmpty(
      field,
      coordinates + west,
      coordinates + northWest,
      coordinates + north,
      coordinates + northEast,
      coordinates + east,
      coordinates + southEast,
      coordinates + south,
      coordinates + southWest
    )
}
