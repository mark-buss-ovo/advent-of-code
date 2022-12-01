package aoc

import scala.io.Source

object Day5 {

  def iterate(instructions: Vector[Int], currentIndex: Int, steps: Int): Int = {
    if (currentIndex < 0 || currentIndex > instructions.length - 1)
      steps
    else {
      val jumpValue = instructions(currentIndex)
      val newInstructions = instructions.updated(currentIndex, jumpValue + 1)
      iterate(newInstructions, currentIndex + jumpValue, steps + 1)
    }
  }

  def iteratePart2(
      instructions: Vector[Int],
      currentIndex: Int,
      steps: Int
  ): Int = {
    if (currentIndex < 0 || currentIndex > instructions.length - 1)
      steps
    else {
      val jumpValue = instructions(currentIndex)
      val newValue = if (jumpValue >= 3) jumpValue - 1 else jumpValue + 1
      val newInstructions = instructions.updated(currentIndex, newValue)
      iteratePart2(newInstructions, currentIndex + jumpValue, steps + 1)
    }
  }

  def main(args: Array[String]): Unit = {
//    val filename = "Day5-sample.txt"
    val filename = "Day5-input.txt"
    val instructions: Vector[Int] =
      Source.fromResource(filename).getLines.map(_.toInt).toVector

//    println(iterate(instructions, 0, 0))
    println(iteratePart2(instructions, 0, 0))
  }
}
