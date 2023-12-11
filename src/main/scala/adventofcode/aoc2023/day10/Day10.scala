package adventofcode.aoc2023.day10
import adventofcode.Problem
import adventofcode.aoc2023.day10.TileType.Ground
import adventofcode.aoc2023.day10.TileType.Horizontal
import adventofcode.aoc2023.day10.TileType.NorthEast
import adventofcode.aoc2023.day10.TileType.NorthWest
import adventofcode.aoc2023.day10.TileType.SouthEast
import adventofcode.aoc2023.day10.TileType.SouthWest
import adventofcode.aoc2023.day10.TileType.Start
import adventofcode.aoc2023.day10.TileType.Vertical
import adventofcode.dfs
import adventofcode.inputLines
import adventofcode.utils.coordinates.Coordinates
import adventofcode.utils.range.*

final case class Day10(input: List[String]) extends Problem[List[String], Int, Int](2023, 10, "Pipe Maze"):
  private val (start, graph, types)       = Tile.parse(input)
  private val loop: Map[Coordinates, Int] = dfs(start, graph, 0)
  private val maxX                        = input.map(_.length).max - 1
  private val maxY                        = input.size - 1

  override def part1: Int = (loop.values.max + 1) / 2
  override def part2: Int =
    val walls = loop
      .groupBy(_._1.y)
      .map:
        case (y, tiles) =>
          y -> tiles.keySet.map(c => Range.single(c.x)).foldLeft(MultiRange.empty[Int])(_ + _)
    var total = 0
    (0 to maxY).foreach: y =>
      var isInside: Boolean            = false
      var lastCorner: Option[TileType] = None
      (0 to maxX).foreach: x =>
        val coordinates = Coordinates(x, y)
        if isInside && !loop.contains(coordinates) then
          println(coordinates)
          total = total + 1
        else if loop.contains(coordinates) then
          val wall = types(coordinates)
          if wall != Horizontal && wall != Ground then
            def toggleInside(): Unit =
              isInside = !isInside
              lastCorner = None
            if wall == Vertical then toggleInside()
            else
              if lastCorner.isEmpty then lastCorner = Some(wall)
              wall match
                case SouthWest if lastCorner.contains(NorthEast) => toggleInside()
                case NorthWest if lastCorner.contains(SouthEast) => toggleInside()
                case SouthWest if lastCorner.contains(SouthEast) => lastCorner = None
                case NorthWest if lastCorner.contains(NorthEast) => lastCorner = None
                case _                                           => ()
    total

object Day10:
  lazy val instance: Day10 = Day10(inputLines("2023/day10.txt"))

object Tile:
  def parse(lines: List[String]): (Coordinates, Map[Coordinates, List[Coordinates]], Map[Coordinates, TileType]) =
    var start: Coordinates = Coordinates(-1, -1)
    val parsed: Map[Coordinates, (TileType, List[Coordinates])] =
      lines.reverse.zipWithIndex
        .flatMap: (line, y) =>
          line.zipWithIndex.map: (char, x) =>
            val isOnFirstLine   = y == 0
            val isOnLastLine    = y == lines.size - 1
            val isOnFirstColumn = x == 0
            val isOnLastColumn  = x == line.length - 1
            val coordinates     = Coordinates(x, y)
            val tileType = char match
              case '|' =>
                (
                  Vertical,
                  if isOnLastLine then List(coordinates.down)
                  else if isOnFirstLine then List(coordinates.up)
                  else List(coordinates.up, coordinates.down)
                )
              case '-' =>
                (
                  Horizontal,
                  if isOnFirstColumn then List(coordinates.right)
                  else if isOnLastColumn then List(coordinates.left)
                  else List(coordinates.left, coordinates.right)
                )
              case 'L' =>
                (
                  NorthEast,
                  if isOnLastLine then List(coordinates.right)
                  else if isOnLastColumn then List(coordinates.up)
                  else List(coordinates.up, coordinates.right)
                )
              case 'J' =>
                (
                  NorthWest,
                  if isOnLastLine then List(coordinates.left)
                  else if isOnFirstColumn then List(coordinates.up)
                  else List(coordinates.up, coordinates.left)
                )
              case '7' =>
                (
                  SouthWest,
                  if isOnFirstLine then List(coordinates.left)
                  else if isOnFirstColumn then List(coordinates.down)
                  else List(coordinates.down, coordinates.left)
                )
              case 'F' =>
                (
                  SouthEast,
                  if isOnFirstLine then List(coordinates.right)
                  else if isOnLastColumn then List(coordinates.down)
                  else List(coordinates.down, coordinates.right)
                )
              case '.' => (Ground, Nil)
              case 'S' =>
                start = coordinates
                (
                  Start,
                  Nil
                )
              case _ => throw new IllegalArgumentException(s"Unknown tile type: $char")
            coordinates -> tileType
        .toMap
    val adjacencyList: Map[Coordinates, List[Coordinates]] = parsed.map:
      case (coordinates, (_, neighbours)) =>
        coordinates -> neighbours
    val tileTypes = parsed.map:
      case (coordinates, (tileType, _)) =>
        coordinates -> tileType
    val startNeighbours =
      List(start.up, start.down, start.left, start.right).filter(adjacencyList.getOrElse(_, Nil).contains(start))
    val neighboursTypes = startNeighbours.map(tileTypes.apply).toSet
    val startType =
      if startNeighbours.toSet == Set(start.up, start.down) then Vertical
      else if startNeighbours.toSet == Set(start.left, start.right) then Horizontal
      else if startNeighbours.toSet == Set(start.down, start.left) then SouthWest
      else if startNeighbours.toSet == Set(start.down, start.right) then SouthEast
      else if startNeighbours.toSet == Set(start.up, start.left) then NorthWest
      else if startNeighbours.toSet == Set(start.up, start.right) then NorthEast
      else throw new IllegalArgumentException(s"Unknown start type: $neighboursTypes")
    (start, adjacencyList ++ Map(start -> startNeighbours), tileTypes ++ Map(start -> startType))
enum TileType:
  case Vertical, Horizontal, NorthWest, NorthEast, SouthWest, SouthEast, Ground, Start
