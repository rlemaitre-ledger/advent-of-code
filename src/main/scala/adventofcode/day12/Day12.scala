package adventofcode.day12

import adventofcode.Coordinates
import adventofcode.Problem
import adventofcode.day12.Day12.*
import adventofcode.inputLines
import scala.annotation.nowarn
import scala.annotation.targetName
import scala.collection.mutable
import scala.collection.mutable.Map as MutableMap

case class Day12(input: HeightMap) extends Problem[HeightMap, Int, Int]("Hill Climbing Algorithm") {
  override def part1: Int = input.minDistanceFromStart.value

  override def part2: Int = input.minDistance.value

}

object Day12 {
  val instance: Day12 = Day12(parse(inputLines("day12.txt")))
  case class Distance(value: Int) {
    def +(other: Distance): Distance = Distance(value + other.value)
  }
  case class Elevation(value: Int) {
    @targetName("minus")
    def -(other: Elevation): Elevation = Elevation(value - other.value)
  }
  enum CellType {
    case Start, Normal, End
  }
  final case class Altitude(height: Char, cellType: CellType) {
    @targetName("minus")
    def -(other: Altitude): Elevation = Elevation(height - other.height)

    val isLowest: Boolean = height == 'a'
  }
  object Altitude {
    def apply(c: Char): Altitude = c match
      case 'S' => Altitude('a', CellType.Start)
      case 'E' => Altitude('z', CellType.End)
      case _   => Altitude(c, CellType.Normal)
  }

  def parse(lines: List[String]): HeightMap =
    HeightMap(
      lines.zipWithIndex.map { case (str, x) =>
        str.zipWithIndex.map { case (alt, y) => Cell(Coordinates(x, y), Altitude(alt)) }.toList
      }
    )

  final case class HeightMap(cells: List[List[Cell]]) {
    private val linesCount                            = cells.size
    private val colsCount                             = cells.map(_.size).max
    private val byCoordinates: Map[Coordinates, Cell] = cells.flatten.map(cell => cell.position -> cell).toMap
    val start: Cell                                   = cells.flatten.find(_.altitude.cellType == CellType.Start).get
    val end: Cell                                     = cells.flatten.find(_.altitude.cellType == CellType.End).get

    def isValid(coordinates: Coordinates): Boolean =
      (0 until linesCount).contains(coordinates.x) && (0 until colsCount).contains(coordinates.y)

    lazy val toGraph: Graph = Graph(adjacency)
    lazy val adjacency: Map[Cell, List[Edge]] =
      val adjacencyMap = MutableMap.empty[Cell, List[Edge]]
      cells.flatten.foreach { cell =>
        cell.position.neighbours
          .filter(isValid)
          .map(byCoordinates.apply)
          .filter(_.isAccessibleFrom(cell))
          .map(to => Edge(cell, to))
          .foreach { edge =>
            adjacencyMap.updateWith(cell) {
              case None        => Some(List(edge))
              case Some(value) => Some(value :+ edge)
            }
          }
      }
      adjacencyMap.toMap

    def minDistanceFromStart: Distance = distanceFrom(Set(start))

    def minDistance: Distance = distanceFrom(possibleStartingPoints)

    def distanceFrom(cell: Set[Cell]): Distance = toGraph.distance(cell, end)

    val lowestCells: List[Cell] = cells.flatten.filter(_.isLowest)
    lazy val possibleStartingPoints: Set[Cell] = adjacency.filter { case (cell, neighbours) =>
      cell.isLowest && neighbours.exists(_.elevation.value == 1)
    }.keySet
  }
  final case class Edge(from: Cell, to: Cell) {
    val distance: Distance   = Distance(1)
    val elevation: Elevation = to.altitude - from.altitude
  }

  final case class Graph(adjacency: Map[Cell, List[Edge]]) {
    def shortestPathsFrom(start: Set[Cell]): ShortestPaths = ShortestPaths.from(start, adjacency)

    def distance(start: Set[Cell], end: Cell): Distance = shortestPathsFrom(start).distanceTo(end)
  }
  object Graph {
    val empty: Graph = Graph(Map.empty)
  }

  case class Path(distance: Distance, predecessor: Option[Cell])

  case class ShortestPaths(from: Set[Cell], directPaths: Map[Cell, Path]) {
    def distanceTo(cell: Cell): Distance = directPaths(cell).distance
  }
  object ShortestPaths {
    @nowarn
    def from(from: Set[Cell], adjacency: Map[Cell, List[Edge]]): ShortestPaths =
      val distanceTo = MutableMap.empty[Cell, Path]
      from.foreach(distanceTo.put(_, Path(Distance(0), None)))
      val sortByDistance: Ordering[Path] = (p1, p2) => p1.distance.value.compareTo(p2.distance.value)
      val initial                        = from.map(c => Path(Distance(0), Some(c))).toList
      val queue                          = mutable.PriorityQueue[Path](initial: _*)(sortByDistance)
      while (queue.nonEmpty)
        val p     = queue.dequeue()
        val edges = adjacency.getOrElse(p.predecessor.get, List.empty)
        edges.foreach { e =>
          val previousDistance = distanceTo(e.from)
          distanceTo.get(e.to) match
            case Some(path) if path.distance.value <= previousDistance.distance.value + e.distance.value => path
            case _ =>
              val path = Path(previousDistance.distance + e.distance, Some(e.from))
              distanceTo.put(e.to, path)
              if (!queue.exists(_.predecessor.contains(e.to))) {
                queue.enqueue(Path(path.distance, Some(e.to)))
              }
        }
      ShortestPaths(from, distanceTo.toMap)
  }
  final case class Cell(position: Coordinates, altitude: Altitude) {
    val isLowest: Boolean = altitude.isLowest

    def isAccessibleFrom(cell: Cell): Boolean =
      position.neighbours.contains(cell.position) && (altitude - cell.altitude).value <= 1
  }
}
