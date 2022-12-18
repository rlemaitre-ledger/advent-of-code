package adventofcode.day08

import Day08.*
import adventofcode.*
case class Day08(input: Field) extends AdventOfCodeBase[Field, Int, Int] {
  override def part1: Int = input.toVisibilityMap.visibleCount

  override def part2: Int =
    input.trees.zipWithIndex.flatMap { case (line, x) =>
      line.zipWithIndex.map { case (tree, y) =>
        input.toBorder(Coordinates(x, y)).scenicScore
      }
    }.max
}
object Day08 {
  val instance: Day08 = Day08(Field.parse(inputLines("day08.txt")))
  final case class Tree(height: Int)

  final case class Field(trees: List[List[Tree]]) {
    val size: Int = trees.length

    def isVisible(pos: Coordinates): Boolean = toBorder(pos).isSeen

    def toBorder(pos: Coordinates): PathToBorder =
      PathToBorder(
        base = treeAt(pos),
        up = (0 until pos.x).toList.reverse.map(l => treeAt(Coordinates(l, pos.y))),
        right = ((pos.y + 1) until size).toList.map(c => treeAt(Coordinates(pos.x, c))),
        down = ((pos.x + 1) until size).toList.map(l => treeAt(Coordinates(l, pos.y))),
        left = (0 until pos.y).toList.reverse.map(c => treeAt(Coordinates(pos.x, c)))
      )

    def treeAt(pos: Coordinates): Tree = trees(pos.x)(pos.y)

    def toVisibilityMap: VisibilityMap =
      VisibilityMap(trees.zipWithIndex.map { case (line, l) =>
        line.zipWithIndex.map { case (_, c) => isVisible(Coordinates(l, c)) }
      })
  }

  object Field {
    def parse(lines: List[String]): Field = Field(lines.map(_.toList.map(_.toString.toInt).map(Tree.apply).toList))
  }

  final case class VisibilityMap(map: List[List[Boolean]]) {
    def visibleCount: Int = map.map(_.count(identity)).sum
  }

  final case class PathToBorder(base: Tree, up: List[Tree], right: List[Tree], down: List[Tree], left: List[Tree]) {
    def any(predicate: Tree => Boolean): Boolean =
      up.forall(predicate) || right.forall(predicate) || down.forall(predicate) || left.forall(predicate)

    def visibleTrees: VisibleTrees =
      val visibleUp    = up.takeWhile(_.height < base.height)
      val visibleRight = right.takeWhile(_.height < base.height)
      val visibleDown  = down.takeWhile(_.height < base.height)
      val visibleLeft  = left.takeWhile(_.height < base.height)
      VisibleTrees(
        up = visibleUp.size + (if (visibleUp.size == up.size) 0 else 1),
        right = visibleRight.size + (if (visibleRight.size == right.size) 0 else 1),
        down = visibleDown.size + (if (visibleDown.size == down.size) 0 else 1),
        left = visibleLeft.size + (if (visibleLeft.size == left.size) 0 else 1)
      )

    def isSeen: Boolean = any(_.height < base.height)

    def scenicScore: Int = visibleTrees.score
  }

  final case class VisibleTrees(up: Int, right: Int, down: Int, left: Int) {
    val score: Int = up * right * down * left
  }
}
