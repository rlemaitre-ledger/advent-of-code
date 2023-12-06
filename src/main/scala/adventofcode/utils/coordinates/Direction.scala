package adventofcode.utils.coordinates

enum Direction:
  case Up, Down, Left, Right

object Direction:
  def parse(str: String): Direction = str match
    case "U" => Up
    case "D" => Down
    case "L" => Left
    case "R" => Right
