package adventofcode.aoc2022.day08

import adventofcode.*
import adventofcode.aoc2022.day08.Day08
import adventofcode.aoc2022.day08.Day08.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import scala.annotation.nowarn

class Day08Test extends AoCTest {
  override val lines: String = """30373
                                 |25512
                                 |65332
                                 |33549
                                 |35390
                                 |""".stripMargin
  val field: Field        = Field.parse(input)
  val testInstance: Day08 = Day08(field)
  test("parse field") {
    assertEquals(
      field,
      Field(
        List(
          List(Tree(3), Tree(0), Tree(3), Tree(7), Tree(3)),
          List(Tree(2), Tree(5), Tree(5), Tree(1), Tree(2)),
          List(Tree(6), Tree(5), Tree(3), Tree(3), Tree(2)),
          List(Tree(3), Tree(3), Tree(5), Tree(4), Tree(9)),
          List(Tree(3), Tree(5), Tree(3), Tree(9), Tree(0))
        )
      )
    )
  }

  property("all border trees are visible") {
    val posGen = Gen.choose(0, 4)
    forAll(posGen) { pos =>
      field.isVisible(Coordinates(pos, 0)): @nowarn
      field.isVisible(Coordinates(0, pos)): @nowarn
      field.isVisible(Coordinates(pos, 4)): @nowarn
      field.isVisible(Coordinates(4, pos)): @nowarn
    }
  }

  property("all border trees have a scenic score of 0") {
    val posGen = Gen.choose(0, 4)
    forAll(posGen) { pos =>
      field.toBorder(Coordinates(pos, 0)).scenicScore == 0: @nowarn
      field.toBorder(Coordinates(0, pos)).scenicScore == 0: @nowarn
      field.toBorder(Coordinates(pos, 4)).scenicScore == 0: @nowarn
      field.toBorder(Coordinates(4, pos)).scenicScore == 0: @nowarn
    }
  }

  test("visibility") {
    assertEquals(field.isVisible(Coordinates(1, 1)), true)
    assertEquals(field.isVisible(Coordinates(1, 2)), true)
    assertEquals(field.isVisible(Coordinates(1, 3)), false)
    assertEquals(field.isVisible(Coordinates(2, 1)), true)
    assertEquals(field.isVisible(Coordinates(2, 2)), false)
    assertEquals(field.isVisible(Coordinates(2, 3)), true)
    assertEquals(field.isVisible(Coordinates(3, 1)), false)
    assertEquals(field.isVisible(Coordinates(3, 2)), true)
    assertEquals(field.isVisible(Coordinates(3, 3)), false)
  }
  test("toBorder") {
    assertEquals(
      field.toBorder(Coordinates(1, 1)),
      PathToBorder(
        base = Tree(5),
        up = List(Tree(0)),
        right = List(Tree(5), Tree(1), Tree(2)),
        down = List(Tree(5), Tree(3), Tree(5)),
        left = List(Tree(2))
      )
    )
  }
  test("part 1") {
    assertEquals(testInstance.part1, 21)
  }
  test("part 2") {
    assertEquals(testInstance.part2, 8)
  }
  test("visible trees") {
    assertEquals(
      field.toBorder(Coordinates(1, 2)).visibleTrees,
      VisibleTrees(up = 1, right = 2, down = 2, left = 1)
    )
    assertEquals(
      field.toBorder(Coordinates(3, 2)).visibleTrees,
      VisibleTrees(up = 2, right = 2, down = 1, left = 2)
    )
  }
  test("answers") {
    assertEquals(Day08.instance.run(Mode.Part1), 1789)
    assertEquals(Day08.instance.run(Mode.Part2), 314820)
  }
}
