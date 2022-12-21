package adventofcode.aoc2022.day21

import scala.annotation.nowarn
import scala.collection.immutable

sealed trait Monkey {
  def name: String
  def value: BigDecimal
  def contains(childName: String): Boolean
  def myValue(myName: String, toMatch: BigDecimal): BigDecimal
}
final case class ConstantMonkey(name: String, value: BigDecimal) extends Monkey {
  override def contains(childName: String): Boolean                     = name == childName
  override def myValue(myName: String, toMatch: BigDecimal): BigDecimal = toMatch
}
final case class OperationMonkey(name: String, first: Monkey, operation: Operation, second: Monkey) extends Monkey {
  override def contains(childName: String): Boolean =
    name == childName || first.contains(childName) || second.contains(childName)
  override def myValue(myName: String, toMatch: BigDecimal): BigDecimal = {
    if first.contains(myName) then
      val newValueToMatch = operation match
        case Operation.Addition       => toMatch - second.value
        case Operation.Subtraction    => toMatch + second.value
        case Operation.Multiplication => toMatch / second.value
        case Operation.Division       => toMatch * second.value
      first.myValue(myName, newValueToMatch)
    else
      val newValueToMatch = operation match
        case Operation.Addition       => toMatch - first.value
        case Operation.Subtraction    => first.value - toMatch
        case Operation.Multiplication => toMatch / first.value
        case Operation.Division       => first.value / toMatch
      second.myValue(myName, newValueToMatch)
  }
  override def value: BigDecimal = operation match
    case Operation.Addition       => first.value + second.value
    case Operation.Subtraction    => first.value - second.value
    case Operation.Multiplication => first.value * second.value
    case Operation.Division       => first.value / second.value
}
object Monkey {
  def parse(lines: List[String]): Map[String, Monkey] = {
    val specsByName = lines.map {
      case s"$name: $first $operation $second" =>
        name -> OperationMonkeySpec(name, first, Operation.parse(operation), second)
      case s"$name: $value" => name -> ConstantMonkeySpec(name, value.toLong)
    }.toMap
    val constants = specsByName.collect { case (_, ConstantMonkeySpec(name, value)) =>
      name -> ConstantMonkey(name, value)
    }
    val queue     = collection.mutable.Queue().enqueueAll(specsByName.removedAll(constants.keys).values)
    val completed = collection.mutable.Map[String, Monkey](constants.toSeq: _*)
    while (queue.nonEmpty) {
      val current: Spec                                       = queue.dequeue()
      val OperationMonkeySpec(name, first, operation, second) = current.asInstanceOf[OperationMonkeySpec]
      if (completed.contains(first) && completed.contains(second)) {
        completed.put(name, OperationMonkey(name, completed(first), operation, completed(second))): @nowarn
      } else {
        queue.enqueue(current): @nowarn
      }
    }
    completed.toMap
  }
  sealed trait Spec
  final case class ConstantMonkeySpec(name: String, value: Long) extends Spec
  final case class OperationMonkeySpec(name: String, firstName: String, operation: Operation, second: String)
      extends Spec
}
