package adventofcode.aoc2022.day22

enum Move {
  case Forward(n: Int)
  case TurnLeft
  case TurnRight
}
object Move {
  def parse(line: String): List[Move] =
    line
      .split('R')
      .map(str =>
        str
          .split('L')
          .map(s => Forward(s.toInt))
          .foldLeft(List.empty[Move])((acc, move) =>
            acc match
              case Nil => List(move)
              case _   => acc ++ List(TurnLeft, move)
          )
      )
      .foldLeft(List.empty[Move])((acc, moves) =>
        acc match
          case Nil => moves
          case _   => acc ++ List(TurnRight) ++ moves
      )
}
