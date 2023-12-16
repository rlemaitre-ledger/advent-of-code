package adventofcode.utils.coordinates

import scala.collection.SeqOps

trait Locatable:
  def coordinates: Coordinates

object Locatable:
  def show[T <: Locatable](width: Int, height: Int)(f: List[T] => String)(seq: Seq[T]): String =
    val lines = (0 until height).map: y =>
      (0 until width).map(x => f(seq.filter(_.coordinates == Coordinates(x, y)).toList)).mkString
    lines.reverse.mkString("\n")
