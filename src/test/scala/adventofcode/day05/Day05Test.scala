package adventofcode.day05

import adventofcode.AoCTest
import adventofcode.Mode
import adventofcode.day05.Day05
import adventofcode.day05.Day05.*

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
  val testInstance: Day05 = Day05(Input.from(input))
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
  val moves: List[Move] = List(
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

  test("ClassicCrane move from empty stack") {
    val cargo = Cargo(ClassicCrane, List(Stack(1), Stack(2)))
    assertEquals(cargo.play(List(Move(1, 0, 1))).top, List(None, None))
  }

  test("CrateMover9000 move from empty stack") {
    val cargo = Cargo(CrateMover9000, List(Stack(1), Stack(2)))
    assertEquals(cargo.play(List(Move(1, 0, 1))).top, List(None, None))
  }

  test("move crates") {
    assertEquals(cargoAtStart.play(List(Move(1, 1, 0))).top, List(Some(Crate("D")), Some(Crate("C")), Some(Crate("P"))))
  }

  test("part 1") {
    assertEquals(testInstance.part1, "CMZ")
  }

  test("part 2") {
    assertEquals(testInstance.part2, "MCD")
  }

  test("Move several crates at once with classic") {
    val stack1 = Stack(1)
    stack1.add(Crate("A"))
    stack1.add(Crate("B"))
    val stack2 = Stack(2)
    val stacks = List(stack1, stack2)
    val move   = Move(2, 0, 1)
    ClassicCrane.move(stacks)(move)
    val expectedStack2 = Stack(2)
    expectedStack2.add(Crate("B"))
    expectedStack2.add(Crate("A"))
    assertEquals(stack1.top, None)
    assertEquals(stack2.top, expectedStack2.top)
  }

  test("Move several crates at once with CrateMover9000") {
    val stack1 = Stack(1)
    stack1.add(Crate("A"))
    stack1.add(Crate("B"))
    val stack2 = Stack(2)
    val stacks = List(stack1, stack2)
    val move   = Move(2, 0, 1)
    CrateMover9000.move(stacks)(move)
    val expectedStack2 = Stack(2)
    expectedStack2.add(Crate("A"))
    expectedStack2.add(Crate("B"))
    assertEquals(stack1.top, None)
    assertEquals(stack2.top, expectedStack2.top)
  }
  test("answers") {
    assertEquals(Day05.instance.run(Mode.Part1), "ZSQVCCJLL")
    assertEquals(Day05.instance.run(Mode.Part2), "QZFJRWHGS")
  }

}
