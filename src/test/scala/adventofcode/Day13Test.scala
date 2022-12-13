package adventofcode

import adventofcode.Day13.*

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
  test("Part 1") {
    intercept[NotImplementedError] {
      assertEquals(part1(input), 13)
    }
  }
  test("Part 2") {
    intercept[NotImplementedError] {
      assertEquals(part2(input), 13)
    }
  }
}
