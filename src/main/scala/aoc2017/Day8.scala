package aoc2017

import scala.io.Source

object Day8 {

  val filenamePart1Sample = "2017/Day8/part1-sample.txt"
  val filenamePart1Input = "2017/Day8/part1-input.txt"

  sealed trait Operation
  object Operation {
    case object Increment extends Operation
    case object Decrement extends Operation
  }

  case class Instruction(
      register: String,
      operation: Operation,
      num: Int,
      condition: String => Boolean
  )

  def part1: Int = {
    ???
//    val instructions =
//      Source.fromResource(filenamePart1Input).getLines.map(line => {})

  }

  def main(args: Array[String]): Unit = {}
}
