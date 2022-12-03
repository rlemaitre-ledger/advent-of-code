package adventofcode.day1

import adventofcode.AdventOfCodeBase
import adventofcode.day1.Day1.caloriesByElf
import scala.io.Source

object Day1 extends AdventOfCodeBase("day1.txt") {

  def caloriesByElf(lines: List[String]): List[(Long, Int)] =
    lines
      .foldLeft(List(0L)) { case (acc, line) =>
        if (line.trim.isEmpty) {
          0L :: acc
        } else {
          (acc.head + line.toLong) :: acc.tail
        }
      }
      .reverse
      .zipWithIndex

  def maxCalories(caloriesByElf: List[(Long, Int)]): Long =
    caloriesByElf.maxBy(_._1)._1

  def top3ElvesCalories(caloriesByElf: List[(Long, Int)]): Long =
    caloriesByElf.sortBy(_._1).reverse.take(3).map(_._1).sum

  def main(args: Array[String]): Unit = {
    val all      = caloriesByElf(input)
    val calories = maxCalories(all)
    println(s"part1: $calories")
    val sumOfTop3Elf = top3ElvesCalories(all)
    println(sumOfTop3Elf)
  }
}
