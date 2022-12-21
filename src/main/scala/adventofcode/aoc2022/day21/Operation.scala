package adventofcode.aoc2022.day21

enum Operation(label: String) {
  case Addition       extends Operation("+")
  case Subtraction    extends Operation("-")
  case Multiplication extends Operation("*")
  case Division       extends Operation("/")
}
object Operation {
  def parse(str: String): Operation = str match
    case "+" => Addition
    case "-" => Subtraction
    case "*" => Multiplication
    case "/" => Division
}
