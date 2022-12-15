package adventofcode

import adventofcode.Day13.*
import org.scalacheck.Prop.forAll
import scala.annotation.nowarn

class Day13Test extends AoCTest {
  override val lines: String = """[1,1,3,1,1]
                                 |[1,1,5,1,1]
                                 |
                                 |[[1],[2,3,4]]
                                 |[[1],4]
                                 |
                                 |[9]
                                 |[[8,7,6]]
                                 |
                                 |[[4,4],4,4]
                                 |[[4,4],4,4,4]
                                 |
                                 |[7,7,7,7]
                                 |[7,7,7]
                                 |
                                 |[]
                                 |[3]
                                 |
                                 |[[[]]]
                                 |[[]]
                                 |
                                 |[1,[2,[3,[4,[5,6,7]]]],8,9]
                                 |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin
  val testSignal: Signal = Signal(
    PacketPair(
      1,
      Data.list(Data.simple(1), Data.simple(1), Data.simple(3), Data.simple(1), Data.simple(1)),
      Data.list(Data.simple(1), Data.simple(1), Data.simple(5), Data.simple(1), Data.simple(1))
    ),
    PacketPair(
      2,
      Data.list(Data.list(Data.simple(1)), Data.list(Data.simple(2), Data.simple(3), Data.simple(4))),
      Data.list(Data.list(Data.simple(1)), Data.simple(4))
    ),
    PacketPair(
      3,
      Data.list(Data.simple(9)),
      Data.list(Data.list(Data.simple(8), Data.simple(7), Data.simple(6)))
    ),
    PacketPair(
      4,
      Data.list(Data.list(Data.simple(4), Data.simple(4)), Data.simple(4), Data.simple(4)),
      Data.list(Data.list(Data.simple(4), Data.simple(4)), Data.simple(4), Data.simple(4), Data.simple(4))
    ),
    PacketPair(
      5,
      Data.list(Data.simple(7), Data.simple(7), Data.simple(7), Data.simple(7)),
      Data.list(Data.simple(7), Data.simple(7), Data.simple(7))
    ),
    PacketPair(
      6,
      Data.list(),
      Data.list(Data.simple(3))
    ),
    PacketPair(
      7,
      Data.list(Data.list(Data.list())),
      Data.list(Data.list())
    ),
    PacketPair(
      8,
      Data.list(
        Data.simple(1),
        Data.list(
          Data.simple(2),
          Data
            .list(Data.simple(3), Data.list(Data.simple(4), Data.list(Data.simple(5), Data.simple(6), Data.simple(7))))
        ),
        Data.simple(8),
        Data.simple(9)
      ),
      Data.list(
        Data.simple(1),
        Data.list(
          Data.simple(2),
          Data
            .list(Data.simple(3), Data.list(Data.simple(4), Data.list(Data.simple(5), Data.simple(6), Data.simple(0))))
        ),
        Data.simple(8),
        Data.simple(9)
      )
    )
  )
  val testInstance: Day13 = Day13(Input(testSignal, packets(input)))
  test("Part 1") {
    assertEquals(testInstance.part1, 13)
  }
  test("Part 2") {
    assertEquals(testInstance.part2, 140)
  }
  test("parse") {
    assertEquals(signal(input), testSignal)
  }
  test("parse empty data list") {
    assertEquals(Data.parse("[]"), Data.list())
  }
  property("parse simple int") {
    forAll { (n: Int) =>
      Data.parse(n.toString) == Data.simple(n)
    }
  }
  test("parse int data list") {
    assertEquals(Data.parse("[1, 2]"), Data.list(Data.simple(1), Data.simple(2)))
  }
  test("parse composite") {
    assertEquals(
      Data.parse("[[1],[2,3,4]]"),
      Data.list(
        Data.list(Data.simple(1)),
        Data.list(
          Data.simple(2),
          Data.simple(3),
          Data.simple(4)
        )
      )
    )
  }
  test("parse composite") {
    assertEquals(
      Data.parse("[[1]]"),
      Data.list(Data.list(Data.simple(1)))
    )
  }
  test("compare empty with non empty") {
    assertEquals(PacketPair.parse(List("[]", "[3]"), 4).isOrdered, true)
  }
  property("compare 2 simple data") {
    forAll { (a: Int, b: Int) =>
      PacketPair(1, Data.simple(a), Data.simple(b)).isOrdered == a <= b
    }
  }
  property("list 1") {
    forAll { (a: List[Int]) =>
      val elements = a.map(Data.simple.apply)
      PacketPair(1, Data.DataList(elements), Data.DataList(elements)).isOrdered
    }
  }
  property("list 2") {
    forAll { (a: List[Int]) =>
      val elements = a.map(Data.simple.apply)
      PacketPair(1, Data.DataList(elements), Data.DataList(elements :+ Data.simple(0))).isOrdered
    }
  }
  property("two list with same element are ok") {
    forAll { (a: List[Int]) =>
      val elements = a.map(Data.simple.apply)
      !PacketPair(1, Data.DataList(elements :+ Data.simple(0)), Data.DataList(elements)).isOrdered
    }
  }
  test("right order indices") {
    assertEquals(signal(input).inRightOrderIndices, List(1, 2, 4, 6))
  }
  test("real test") {
    val left = Data.parse(
      "[[[[6,2,3,9,9],[4,1],10,[],3],[4,1,[8,5,5,8],[6,7,2],[4]]],[[[10,4,2,10,9],[2],[2,4,6],3],[[0,8,10],[4],[0,7,0,9,0]],[3]],[4,7,3,8]]"
    )
    val right = Data.parse("[[[6,5,0],9,5,7,2],[[[],[],7,3,0]],[[7],[],4],[1,6,6,[],9]]")
    assertEquals(PacketPair(150, left, right).isOrdered, false)
  }
  test("real test 2") {
    val left = Data.parse(
      "[[[1,[5,2,6,6,0]],[[7,7],[]],[[4,5],[],[],[3,7,1,8],1]],[9,[[],[8,7,8,9]],[2,4,0,[7,4,4]],[1],[]],[[[],[4,7,0,4],[1,0]],5,4],[],[[9,[],[7]]]]"
    )
    val right = Data.parse("[[[3],[[5,2,1]],2]]")
    assertEquals(PacketPair(150, left, right).isOrdered, true)
  }
  test("real test 3") {
    val left = Data.parse(
      "[[[[5,8,8,6],0]]]"
    )
    val right = Data.parse(
      "[[[[3,0,10,1,5],10,[7,0]],8,[0,9,4,[8,10]],[[6],[4,4,8],7,9,[9]]],[[7,[],0,2],[[]],5,6,[]],[0,[[1,10,8,8,1],2,[2,6],[10,9],[7]]],[2],[[[],2,5,[2,9,6,0],8],9]]"
    )
    assertEquals(PacketPair(150, left, right).isOrdered, false)
  }
  test("answer") {
    assertEquals(Day13.instance.run(Mode.Part1), 5198)
    assertEquals(Day13.instance.run(Mode.Part2), 22344)
  }
  test("parse invalid input") {
    intercept[IllegalArgumentException] {
      DataParser.parseString("toto")
    }
  }
  test("compare to") {
    assertEquals(Data.simple(0).compareTo(Data.simple(0)), 0)
    assertEquals(Data.simple(1).compareTo(Data.simple(0)), 1)
    assertEquals(Data.simple(0).compareTo(Data.simple(1)), -1)
  }
}
