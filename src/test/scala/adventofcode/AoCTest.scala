package adventofcode

import munit.FunSuite
import munit.ScalaCheckSuite
trait AoCTest extends FunSuite, ScalaCheckSuite {
  val lines: String
  def input: List[String] = lines.split('\n').toList
}
