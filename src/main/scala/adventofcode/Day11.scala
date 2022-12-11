package adventofcode

import adventofcode.Day11.monkeys

object Day11 extends AdventOfCodeBase[Long, Long]("day11.txt") {
  override def part1(lines: List[String]): Long = business(play(monkeys(lines, Mode.Part1), 20))
  override def part2(lines: List[String]): Long = business(play(monkeys(lines, Mode.Part2), 10000))
  def monkeys(lines: List[String], mode: Mode): List[Monkey] = {
    val monkeys  = lines.grouped(7).map(l => Monkey.parse(l, mode)).toList.sortBy(_.number)
    val dividers = monkeys.map(_.test.divisibleBy).toSet
    monkeys.map(m => m.copy(items = m.items.map(item => dividers.foldLeft(item)((i, mod) => i.addModulo(mod)))))
  }
  def business(monkeys: List[Monkey]): Long = monkeys.map(_.inspections).sorted.reverse.take(2).product
  def play(monkeys: List[Monkey], nbRounds: Int): List[Monkey] =
    (1 to nbRounds).foldLeft(monkeys)((m, _) => round(m))
  def round(monkeys: List[Monkey]): List[Monkey] = {
    def roundFor(state: List[Monkey], n: Int): List[Monkey] = {
      val current = state(n)
      val result  = current.inspectItems()
      result.moves.foldLeft(state.updated(n, result.updated)) { case (list, (index, toMove)) =>
        list.updated(index, list(index).receive(toMove))
      }
    }
    monkeys.indices.foldLeft(monkeys)(roundFor)
  }

  final case class Monkey(number: Int, items: List[Item], operation: Operation, test: Test, inspections: Long) {
    def inspectItems(): InspectionResult =
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
    def parse(input: List[String], mode: Mode): Monkey =
      Monkey(
        number = parseNumber(input.head),
        items = Item.parse(input(1), mode),
        operation = Operation.parse(input(2)),
        test = Test.parse(input(3), input(4), input(5)),
        inspections = 0
      )
    def parseNumber(str: String): Int = str match
      case s"Monkey ${nb}:" => nb.toInt
  }
  sealed trait Item {
    def add(increment: Int): Item
    def multiply(factor: Int): Item
    def square: Item
    def isMultipleOf(nb: Int): Boolean
    def addModulo(mod: Int): Item
    def afterInspection: Item
  }
  final case class SimpleItem(worryLevel: Int) extends Item {
    override def add(increment: Int): Item      = SimpleItem(worryLevel + increment)
    override def multiply(factor: Int): Item    = SimpleItem(worryLevel * factor)
    override def square: Item                   = SimpleItem(worryLevel * worryLevel)
    override def isMultipleOf(nb: Int): Boolean = worryLevel % nb == 0
    override def afterInspection: Item          = SimpleItem(worryLevel / 3)
    override def addModulo(mod: Int): Item      = this
  }
  final case class ModuloItem(initialValue: Int, dividers: Map[Int, Int]) extends Item {
    override def add(increment: Int): Item      = copy(dividers = dividers.map((k, v) => (k, (v + increment) % k)))
    override def multiply(factor: Int): Item    = copy(dividers = dividers.map((k, v) => (k, (v * factor) % k)))
    override def square: Item                   = copy(dividers = dividers.map((k, v) => (k, (v * v) % k)))
    override def isMultipleOf(nb: Int): Boolean = dividers.get(nb).contains(0)
    override def afterInspection: Item          = this
    override def addModulo(mod: Int): Item      = copy(dividers = dividers.updated(mod, initialValue % mod))
  }
  object ModuloItem {
    def apply(n: Int): ModuloItem = ModuloItem(n, Map.empty)
  }
  object Item {
    def parse(str: String, mode: Mode): List[Item] = str match
      case s"  Starting items: ${levels}" =>
        levels
          .split(',')
          .map(s => s.trim.toInt)
          .map(n =>
            mode match
              case Mode.Part1 => SimpleItem(n)
              case Mode.Part2 => ModuloItem(n)
          )
          .toList
  }
  sealed trait Operation {
    def modify(item: Item): Item
  }
  case class Increase(increment: Int) extends Operation {
    override def modify(item: Item): Item = item.add(increment)
  }
  case class Multiply(factor: Int) extends Operation {
    override def modify(item: Item): Item = item.multiply(factor)
  }
  case object Square extends Operation {
    override def modify(item: Item): Item = item.square
  }
  object Operation {
    def parse(str: String): Operation = str match
      case s"  Operation: new = old * old"          => Square
      case s"  Operation: new = old * ${factor}"    => Multiply(factor.toInt)
      case s"  Operation: new = old + ${increment}" => Increase(increment.toInt)
  }
  case class Test(divisibleBy: Int, ifTrue: Int, ifFalse: Int) {
    def test(item: Item): Int = if (item.isMultipleOf(divisibleBy)) ifTrue else ifFalse
  }
  object Test {
    def parse(testLine: String, ifTrueLine: String, ifFalseLine: String): Test =
      Test(parseTest(testLine), parseAction(ifTrueLine), parseAction(ifFalseLine))
    def parseTest(str: String): Int = str match
      case s"  Test: divisible by ${nb}" => nb.toInt
    def parseAction(str: String): Int = str match
      case s"    If ${_}: throw to monkey ${monkey}" => monkey.toInt
  }
  enum Mode {
    case Part1, Part2
  }
}
