package adventofcode.day05

import adventofcode.AdventOfCodeBase
import scala.annotation.nowarn
import scala.collection.mutable
import scala.util.matching.Regex

object Day05 extends AdventOfCodeBase[String]("day05.txt") {
  def initCargo(lines: List[String]): Cargo = Cargo.from(lines.filterNot(_.startsWith("move")).filterNot(_.isBlank))

  def moves(lines: List[String]): List[Move] = lines.filter(_.startsWith("move")).map(Move.from)

  def parse(lines: List[String]): (Cargo, List[Move]) = (initCargo(lines), moves(lines))

  override def part1(lines: List[String]): String = {
    val (cargo: Cargo, moves: List[Move]) = parse(lines)
    cargo.play(moves).topState
  }

  override def part2(lines: List[String]): String = ???
}

final case class Cargo(stacks: List[Stack]) {
  def play(moves: List[Move]): Cargo = {
    moves.foreach { case Move(count, from, to) =>
      (0 until count).foreach { _ =>
        stacks(from).pop() match
          case Some(crate) => stacks(to).add(crate)
          case None        =>
      }
    }
    this
  }

  def top: List[Option[Crate]]                 = stacks.map(_.top)
  def add(stackIndex: Int, crate: Crate): Unit = stacks(stackIndex).add(crate)
  def topState: String                         = top.map(_.map(_.name).getOrElse("")).mkString
}

object Cargo {
  def from(lines: List[String]): Cargo = {
    lines.reverse match
      case head :: tail => {
        val stacks = head.grouped(4).map(_.trim.toInt).map(Stack.apply).toList
        for {
          line           <- tail
          (crate, index) <- line.grouped(4).map(_.trim).toList.zipWithIndex
        } do {
          if (crate.nonEmpty) stacks(index).add(Crate.from(crate))
        }
        Cargo(stacks)
      }
      case Nil => Cargo(Nil)
  }

}

final case class Stack(index: Int) {
  private var stack: List[Crate] = Nil
  @nowarn
  def add(crate: Crate): Unit = {
    stack = crate :: stack
  }
  def pop(): Option[Crate] = stack match
    case head :: tail => {
      stack = tail
      Some(head)
    }
    case Nil => None

  def top: Option[Crate] = stack.headOption
}

final case class Crate(name: String) extends AnyVal

object Crate {
  def from(input: String): Crate = Crate(input.drop(1).dropRight(1))
}

final case class Move(count: Int, from: Int, to: Int)

object Move {
  def from(line: String): Move = line match
    case pattern(count, from, to) => Move(count.toInt, from.toInt - 1, to.toInt - 1)

  val pattern: Regex = """move ([0-9]+) from ([0-9]+) to ([0-9]+)""".r
}
