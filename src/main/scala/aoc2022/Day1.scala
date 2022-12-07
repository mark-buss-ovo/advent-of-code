package aoc2022

import scala.io.Source

object Day1 {

  val filenamePart1Sample = "2022/Day1/part1-sample.txt"
  val filenamePart1Input = "2022/Day1/part1-input.txt"

  def getCounts: Vector[Int] = {
    val lines = Source.fromResource(filenamePart1Input).getLines.toVector

    lines.foldLeft(Vector(0))((calorieCounts, line) => {
      if (line.isEmpty) {
        calorieCounts.appended(0)
      } else {
        val calories = line.toInt
        val currentCalories = calorieCounts(calorieCounts.length - 1)
        calorieCounts.updated(
          calorieCounts.length - 1,
          currentCalories + calories
        )
      }
    })
  }

  def part1: Int = {
    getCounts.max
  }

  def part2: Int = {
    getCounts.sorted.reverse.take(3).sum
  }

  def main(args: Array[String]): Unit = {
    println(part1)
    println(part2)
  }
}
