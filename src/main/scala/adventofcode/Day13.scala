package adventofcode

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.*

object Day13 extends AdventOfCodeBase[Int, Int]("day13.txt") {
  override def part1(lines: List[String]): Int = signal(lines).inRightOrderIndices.sum

  override def part2(lines: List[String]): Int = {
    val sorted = (packets(lines) ++ List(Data.divider1, Data.divider2)).sorted.zipWithIndex
    sorted.filter(p => List(Data.divider1, Data.divider2).contains(p._1)).map(_._2 + 1).product
  }

  def signal(lines: List[String]): Signal      = Signal.parse(lines)
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
    def isOrdered: Boolean = PacketPair.compare(left, right) != ComparisonResult.Higher
  }
  object PacketPair {
    import ComparisonResult.*
    import Data.*
    def parse(lines: List[String], index: Int): PacketPair =
      PacketPair(index + 1, Data.parse(lines.head), Data.parse(lines(1)))

    def compare(left: Data, right: Data): ComparisonResult = (left, right) match {
      case (SimpleData(first), SimpleData(second)) =>
        if (first == second) Equals else if (first < second) Lower else Higher
      case (DataList(head1 :: tail1), DataList(head2 :: tail2)) =>
        val headCompare = compare(head1, head2)
        headCompare match
          case ComparisonResult.Equals => compare(DataList(tail1), DataList(tail2))
          case _                       => headCompare
      case (DataList(Nil), DataList(Nil))        => Equals
      case (DataList(Nil), _: DataList)          => Lower
      case (_: DataList, DataList(Nil))          => Higher
      case (first: SimpleData, second: DataList) => compare(Data.list(first), second)
      case (first: DataList, second: SimpleData) => compare(first, Data.list(second))
    }
  }
  enum ComparisonResult {
    case Lower, Equals, Higher
  }
  enum Data extends Comparable[Data] {
    case DataList(list: List[Data])
    case SimpleData(value: Int)

    override def compareTo(o: Data): Int = {
      PacketPair.compare(this, o) match
        case ComparisonResult.Lower  => -1
        case ComparisonResult.Equals => 0
        case ComparisonResult.Higher => 1
    }
  }
  object Data {
    def parse(str: String): Data = DataParser.parseString(str)
    def simple(n: Int): Data     = Data.SimpleData(n)
    def list(l: Data*): Data     = Data.DataList(l.toList)
    val divider1: Data           = Data.list(Data.list(Data.simple(2)))
    val divider2: Data           = Data.list(Data.list(Data.simple(6)))
  }
  object DataParser extends JavaTokenParsers {
    override def skipWhitespace: Boolean = true

    def simple: Parser[Data]   = wholeNumber ^^ { i => Data.simple(i.toInt) }
    def datalist: Parser[Data] = "[" ~> repsep(data, ",") <~ "]" ^^ { Data.list }
    def data: Parser[Data]     = simple | datalist
    def parseString(str: String): Data = parse[Data](data, str) match
      case Success(result, _) => result
      case _: NoSuccess       => throw new IllegalArgumentException
  }

}
