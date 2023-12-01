package adventofcode.aoc2022.day24

import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Direction

final case class Valley(
    tiles: Map[Coordinates, Set[Tile]],
    entry: Coordinates,
    exit: Coordinates,
    nbLines: Int,
    nbColumn: Int
) {
  def move(tile: Tile, from: Coordinates, to: Coordinates): Valley =
    copy(
      tiles = tiles
        .updatedWith(from) {
          case Some(value) => Some(value - tile)
          case None        => Some(Set.empty)
        }
        .updatedWith(to) {
          case Some(value) => Some(value + tile)
          case None        => Some(Set(tile))
        }
    )
  def isWall(coordinates: Coordinates): Boolean = tiles(coordinates).contains(Tile.Wall)

  def next: Valley =
    tiles.toSeq
      .flatMap { case (c: Coordinates, ts: Set[Tile]) =>
        ts.map(t => (c, t)).toSeq
      }
      .foldLeft(this) { case (acc, (from, tile)) =>
        tile match
          case Tile.Wall => acc
          case Tile.Wind(direction) =>
            acc.move(
              Tile.Wind(direction),
              from,
              direction match
                case Direction.Up =>
                  val to = from + Coordinates.north
                  if acc.isWall(to) || to.y >= nbLines then {
                    val lower = acc.tiles.keys.filter(_.x == from.x).minBy(_.y)
                    if acc.isWall(lower) then lower + Coordinates.north else lower
                  } else to
                case Direction.Down =>
                  val to = from + Coordinates.south
                  if acc.isWall(to) || to.y <= 0 then {
                    val upper = acc.tiles.keys.filter(_.x == from.x).maxBy(_.y)
                    if acc.isWall(upper) then upper + Coordinates.south else upper
                  } else to
                case Direction.Left =>
                  val to = from + Coordinates.west
                  if acc.isWall(to) || to.x >= 0 then {
                    val leftMost = acc.tiles.keys.filter(_.y == from.y).maxBy(_.x)
                    if acc.isWall(leftMost) then leftMost + Coordinates.west else leftMost
                  } else to
                case Direction.Right =>
                  val to = from + Coordinates.east
                  if acc.isWall(to) || to.x >= nbColumn then {
                    val rightMost = acc.tiles.keys.filter(_.y == from.y).minBy(_.x)
                    if acc.isWall(rightMost) then rightMost + Coordinates.east else rightMost
                  } else to
            )
      }
}
object Valley {
  def parse(lines: List[String]): Valley = {
    val tiles: Map[Coordinates, Set[Tile]] = lines.zipWithIndex.flatMap { (line, lineIndex) =>
      line.zipWithIndex.map((c, i) => Coordinates(i, lines.length - 1 - lineIndex) -> Tile.parse(c))
    }.toMap
    val start = tiles
      .find { (c, t) =>
        c.y == lines.length - 1 && t.isEmpty
      }
      .get
      ._1
    val end = tiles
      .find { (c, t) =>
        c.y == 0 && t.isEmpty
      }
      .get
      ._1
    Valley(tiles, start, end, lines.length, lines.head.length)
  }
}
