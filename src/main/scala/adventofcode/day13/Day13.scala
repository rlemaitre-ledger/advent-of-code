package adventofcode.day13

import adventofcode.Problem
import adventofcode.day13.Day13.*
import adventofcode.inputLines
import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.*

case class Day13(input: Input) extends Problem[Input, Int, Int]("Distress Signal") {
  override def part1: Int = input.signal.inRightOrderIndices.sum

  override def part2: Int =
    val sorted = (input.packets ++ List(Data.divider1, Data.divider2)).sorted.zipWithIndex
    sorted.filter(p => List(Data.divider1, Data.divider2).contains(p._1)).map(_._2 + 1).product
}
object Day13 {
  val instance: Day13 = {
    val lines = inputLines("day13.txt")
    Day13(Input(signal(lines), packets(lines)))
  }
  final case class Input(signal: Signal, packets: List[Data])

  def signal(lines: List[String]): Signal = Signal.parse(lines)

  def packets(lines: List[String]): List[Data] = lines.filter(_.nonEmpty).map(Data.parse)

  case class Signal(packets: PacketPair*) {
    def inRightOrderIndices: Seq[Int] = packets.filter(_.isOrdered).map(_.index)
  }

  object Signal {
    def parse(lines: List[String]): Signal = Signal(
      lines.grouped(3).map(_.take(2)).zipWithIndex.map(PacketPair.parse).toList: _*
    )
  }
  case class PacketPair(index: Int, left: Data, right: Data) {
    def isOrdered: Boolean = left.compareTo(right) <= 0
  }
  object PacketPair {

    import Data.*

    def parse(lines: List[String], index: Int): PacketPair =
      PacketPair(index + 1, Data.parse(lines.head), Data.parse(lines(1)))

    def compare(left: Data, right: Data): Int = (left, right) match
      case (SimpleData(first), SimpleData(second)) => first.compareTo(second)
      case (DataList(head1 :: tail1), DataList(head2 :: tail2)) =>
        val headCompare = compare(head1, head2)
        if (headCompare == 0) compare(DataList(tail1), DataList(tail2)) else headCompare
      case (DataList(Nil), DataList(Nil))        => 0
      case (DataList(Nil), _: DataList)          => -1
      case (_: DataList, DataList(Nil))          => 1
      case (first: SimpleData, second: DataList) => compare(Data.list(first), second)
      case (first: DataList, second: SimpleData) => compare(first, Data.list(second))
  }
  enum Data extends Comparable[Data] {
    case DataList(list: List[Data])
    case SimpleData(value: Int)

    override def compareTo(o: Data): Int = PacketPair.compare(this, o)
  }
  object Data {
    def parse(str: String): Data = DataParser.parseString(str)

    def simple(n: Int): Data = Data.SimpleData(n)

    def list(l: Data*): Data = Data.DataList(l.toList)

    val divider1: Data = Data.list(Data.list(Data.simple(2)))
    val divider2: Data = Data.list(Data.list(Data.simple(6)))
  }
  object DataParser extends JavaTokenParsers {
    override def skipWhitespace: Boolean = true

    def simple: Parser[Data] = wholeNumber ^^ { i => Data.simple(i.toInt) }

    def datalist: Parser[Data] = "[" ~> repsep(data, ",") <~ "]" ^^ {
      Data.list
    }

    def data: Parser[Data] = simple | datalist

    def parseString(str: String): Data = parse[Data](data, str) match
      case Success(result, _) => result
      case _: NoSuccess       => throw new IllegalArgumentException
  }
}
