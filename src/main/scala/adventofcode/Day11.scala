package adventofcode

import adventofcode.Day11.monkeys

object Day11 extends AdventOfCodeBase[Int, Int]("day11.txt") {
  override def part1(lines: List[String]): Int = business(play(monkeys(lines), 20))
  override def part2(lines: List[String]): Int = ???
  def monkeys(lines: List[String]): List[Monkey] =
    lines.grouped(7).map(Monkey.parse).toList.sortBy(_.number)
  def business(monkeys: List[Monkey]): Int = monkeys.map(_.inspections).sorted.reverse.take(2).product
  def play(monkeys: List[Monkey], nbRounds: Int): List[Monkey] =
    (1 to nbRounds).foldLeft(monkeys)((m, _) => round(m))
  def round(monkeys: List[Monkey]): List[Monkey] = {
    def roundFor(state: List[Monkey], n: Int): List[Monkey] = {
      val current = state(n)
      val result  = current.inspectItems
      result.moves.foldLeft(state.updated(n, result.updated)) { case (list, (index, toMove)) =>
        list.updated(index, list(index).receive(toMove))
      }
    }
    monkeys.indices.foldLeft(monkeys)(roundFor)
  }

  final case class Monkey(number: Int, items: List[Item], operation: Operation, test: Test, inspections: Int) {
    def inspectItems: InspectionResult =
      InspectionResult(
        copy(items = List.empty, inspections = inspections + items.size),
        items.map(operation.modify).map(_.afterInspection).foldLeft(Map.empty[Int, List[Item]]) { (moves, item) =>
          val to = test.test(item)
          moves.updatedWith(to) {
            case Some(list) => Some(list :+ item)
            case None       => Some(List(item))
          }
        }
      )
    def receive(list: List[Item]): Monkey = copy(items = items ++ list)
  }
  final case class InspectionResult(updated: Monkey, moves: Map[Int, List[Item]])
  object Monkey {
    def parse(input: List[String]): Monkey =
      Monkey(
        number = parseNumber(input.head),
        items = Item.parse(input(1)),
        operation = Operation.parse(input(2)),
        test = Test.parse(input(3), input(4), input(5)),
        inspections = 0
      )
    def parseNumber(str: String): Int = str match
      case s"Monkey ${nb}:" => nb.toInt
  }
  final case class Item(worryLevel: Int) {
    def afterInspection: Item = Item(worryLevel / 3)
  }
  object Item {
    def parse(str: String): List[Item] = str match
      case s"  Starting items: ${levels}" => levels.split(',').map(_.trim.toInt).map(Item.apply).toList

  }
  sealed trait Operation {
    def modify(item: Item): Item
  }
  case class Increase(increment: Int) extends Operation {
    override def modify(item: Item): Item = Item(item.worryLevel + increment)
  }
  case class Multiply(factor: Int) extends Operation {
    override def modify(item: Item): Item = Item(item.worryLevel * factor)
  }
  case object Square extends Operation {
    override def modify(item: Item): Item = Item(item.worryLevel * item.worryLevel)
  }
  object Operation {
    def parse(str: String): Operation = str match
      case s"  Operation: new = old * old"          => Square
      case s"  Operation: new = old * ${factor}"    => Multiply(factor.toInt)
      case s"  Operation: new = old + ${increment}" => Increase(increment.toInt)
  }
  case class Test(divisibleBy: Int, ifTrue: Int, ifFalse: Int) {
    def test(item: Item): Int = if (item.worryLevel % divisibleBy == 0) ifTrue else ifFalse
  }
  object Test {
    def parse(testLine: String, ifTrueLine: String, ifFalseLine: String): Test =
      Test(parseTest(testLine), parseAction(ifTrueLine), parseAction(ifFalseLine))
    def parseTest(str: String): Int = str match
      case s"  Test: divisible by ${nb}" => nb.toInt
    def parseAction(str: String): Int = str match
      case s"    If ${_}: throw to monkey ${monkey}" => monkey.toInt
  }
}
