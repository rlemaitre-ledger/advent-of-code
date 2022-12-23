package adventofcode.aoc2022.day22

import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.coordinates.Direction
import scala.annotation.nowarn

final case class Board(tiles: Map[Coordinates, Tile]) {
  val start: Coordinates = tiles.keySet.filter(_.y == 1).minBy(_.x)
}
object Board {
  @nowarn
  def parse(lines: List[String]): Board = {
    val tiles: Map[Coordinates, Tile] = lines.zipWithIndex
      .flatMap((line, lineIndex) =>
        line.zipWithIndex
          .map { (c, column) =>
            Tile.parse(Coordinates(column + 1, lineIndex + 1), c)
          }
          .filter(_.isDefined)
          .map(_.get)
      )
      .map(t => t.coordinates -> t)
      .toMap
    tiles.values.foreach { tile =>
      val toUp = tile.coordinates.move(Direction.Down)
      tile.up =
        if tiles.contains(toUp) then tiles(toUp).coordinates
        else tiles(tiles.keySet.filter(c => c.x == tile.coordinates.x).maxBy(_.y)).coordinates
      val toDown = tile.coordinates.move(Direction.Up)
      tile.down =
        if tiles.contains(toDown) then tiles(toDown).coordinates
        else tiles(tiles.keySet.filter(c => c.x == tile.coordinates.x).minBy(_.y)).coordinates
      val toRight = tile.coordinates.move(Direction.Right)
      tile.right =
        if tiles.contains(toRight) then tiles(toRight).coordinates
        else tiles(tiles.keySet.filter(c => c.y == tile.coordinates.y).minBy(_.x)).coordinates
      val toLeft = tile.coordinates.move(Direction.Left)
      tile.left =
        if tiles.contains(toLeft) then tiles(toLeft).coordinates
        else tiles(tiles.keySet.filter(c => c.y == tile.coordinates.y).maxBy(_.x)).coordinates
    }
    Board(tiles)
  }
}
