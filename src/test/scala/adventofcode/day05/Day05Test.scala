package adventofcode.day05

import adventofcode.AoCTest

class Day05Test extends AoCTest {
  override val lines: String = """    [D]    
                                 |[N] [C]    
                                 |[Z] [M] [P]
                                 | 1   2   3 
                                 |
                                 |move 1 from 2 to 1
                                 |move 3 from 1 to 3
                                 |move 2 from 2 to 1
                                 |move 1 from 1 to 2""".stripMargin
  def cargoAtStart: Cargo = {
    val cargo: Cargo = Cargo(ClassicCrane, List(Stack(1), Stack(2), Stack(3)))
    cargo.add(0, Crate("Z"))
    cargo.add(0, Crate("N"))
    cargo.add(1, Crate("M"))
    cargo.add(1, Crate("C"))
    cargo.add(1, Crate("D"))
    cargo.add(2, Crate("P"))
    cargo
  }
  val moves = List(
    Move(1, 1, 0),
    Move(3, 0, 2),
    Move(2, 1, 0),
    Move(1, 0, 1)
  )

  test("stack add and top") {
    val stack = Stack(1)
    assertEquals(stack.top, None)
    stack.add(Crate("A"))
    assertEquals(stack.top, Some(Crate("A")))
    stack.add(Crate("B"))
    assertEquals(stack.top, Some(Crate("B")))
    val b = stack.pop()
    assertEquals(b, Some(Crate("B")))
    assertEquals(stack.top, Some(Crate("A")))
  }

  test("Cargo init") {
    assertEquals(Cargo.from(ClassicCrane, Nil), Cargo(ClassicCrane, Nil))
    val cargo = Day05.initCargo(ClassicCrane, input)
    assertEquals(cargo, cargoAtStart)
    assertEquals(cargo.top, List(Some(Crate("N")), Some(Crate("D")), Some(Crate("P"))))
  }

  test("moves init") {
    assertEquals(Day05.moves(input), moves)
  }

  test("parse") {
    assertEquals(Day05.parse(ClassicCrane, input), (cargoAtStart, moves))
  }

  test("Move from empty stack") {
    val cargo = Cargo(ClassicCrane, List(Stack(1), Stack(2)))
    assertEquals(cargo.play(List(Move(1, 0, 1))).top, List(None, None))
  }
  test("move crates") {
    assertEquals(cargoAtStart.play(List(Move(1, 1, 0))).top, List(Some(Crate("D")), Some(Crate("C")), Some(Crate("P"))))
  }

  test("part 1") {
    assertEquals(Day05.part1(input), "CMZ")
  }

}
