package adventofcode

import scala.annotation.targetName
import scala.util.matching.Regex

object Day10 extends AdventOfCodeBase[Int]("day10.txt") {
  override def part1(lines: List[String]): Int = {
    val execs = executions(lines)
    List(20, 60, 100, 140, 180, 220)
      .map(Cycle.apply)
      .map(signal(_, execs))
      .map(_.strength)
      .sum
  }

  override def part2(lines: List[String]): Int = ???

  def instructions(lines: List[String]): List[Instruction] = lines.map(Instruction.parse)
  def executions(lines: List[String]): List[Execution] =
    instructions(lines)
      .foldLeft((List.empty[Execution], Cycle(0))) { case ((instructions, previousCycle), instruction) =>
        val tick      = previousCycle.next
        val execution = Execution(tick, instruction)
        (instructions :+ execution, execution.endsAt)
      }
      ._1
  def registerAt(cycle: Cycle, executions: List[Execution]): Register =
    executions
      .filter(_.endsAt.number < cycle.number)
      .foldLeft(Register.init)((register, execution) => execution.instruction.execute(register))

  def signal(cycle: Cycle, executions: List[Execution]): Signal =
    Signal.from(cycle, registerAt(cycle, executions))

  case class Signal(strength: Int)
  object Signal {
    def from(cycle: Cycle, register: Register): Signal = {
      Signal(cycle.number * register.value)
    }
  }
  enum Instruction(val nbCycles: Int) {
    case Noop           extends Instruction(0)
    case Addx(inc: Int) extends Instruction(1)
  }
  object Instruction {
    private val pattern: Regex = "addx (.*)".r
    def parse(str: String): Instruction = str match
      case "noop"         => Noop
      case pattern(value) => Addx(value.toInt)
  }
  extension (i: Instruction) {
    def execute(register: Register): Register = i match
      case Instruction.Noop      => register
      case Instruction.Addx(inc) => register + inc
  }
  case class Execution(startsAt: Cycle, instruction: Instruction) {
    val endsAt: Cycle = startsAt + instruction.nbCycles
  }
  case class Cycle(number: Int) {
    def next: Cycle = copy(number = number + 1)
    @targetName("plus")
    def +(inc: Int): Cycle = copy(number = number + inc)
  }
  case class Register(value: Int) {
    @targetName("plus")
    def +(inc: Int): Register = copy(value = value + inc)
  }
  object Register {
    val init: Register = Register(1)
  }
}
