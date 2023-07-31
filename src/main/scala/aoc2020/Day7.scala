package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day7 {
  val filenamePart1Sample = "2020/Day7/part1-sample.txt"
  val filenamePart1Input = "2020/Day7/part1-input.txt"

  sealed trait Instruction

  private case class NoOp(num: Int) extends Instruction
  private case class Acc(num: Int) extends Instruction
  private case class Jmp(num: Int) extends Instruction

  private def parseInstructions(filename: String): List[Instruction] = {
    Source
      .fromResource(filename)
      .getLines()
      .map(line => {
        val parts = line.split(' ')
        val num = parts(1).toInt
        parts(0) match {
          case "nop" => NoOp(num)
          case "acc" => Acc(num)
          case "jmp" => Jmp(num)
        }
      })
      .toList
  }

  private def findAccumulatorValue(
      instructions: List[Instruction],
      part2: Boolean
  ): Option[Int] = {

    @tailrec
    def executeNext(index: Int, visited: List[Int], acc: Int): Option[Int] = {
      if (visited.contains(index)) {
        if (part2) None else Some(acc)
      } else if (part2 && index == instructions.size) {
        Some(acc)
      } else {
        val instruction = instructions(index)
        instruction match {
          case NoOp(_) => executeNext(index + 1, visited.appended(index), acc)
          case Acc(num) =>
            executeNext(index + 1, visited.appended(index), acc + num)
          case Jmp(num) =>
            executeNext(index + num, visited.appended(index), acc)
        }
      }
    }

    executeNext(0, List.empty, 0)
  }

  private def findAccWhenLooping(filename: String): Int = {
    findAccumulatorValue(parseInstructions(filename), part2 = false).get
  }

  private def getAllCombinations(filename: String): List[List[Instruction]] = {
    val baseInstructions = parseInstructions(filename)

    baseInstructions.indices.foldLeft(List.empty[List[Instruction]])(
      (combos, index) => {
        val thisInstruction = baseInstructions(index)
        thisInstruction match {
          case NoOp(num) =>
            combos.appended(baseInstructions.updated(index, Jmp(num)))
          case Jmp(num) =>
            combos.appended(baseInstructions.updated(index, NoOp(num)))
          case _ => combos
        }
      }
    )
  }

  private def findAccWhenFixed(filename: String): Int = {
    val multiverse = getAllCombinations(filename)

    val accWhenFixed =
      multiverse.view.map(findAccumulatorValue(_, part2 = true)).collectFirst {
        case Some(num) => num
      }

    accWhenFixed.get

  }

  def main(args: Array[String]): Unit = {
    println(findAccWhenLooping(filenamePart1Sample))
    println(findAccWhenLooping(filenamePart1Input))

    println(findAccWhenFixed(filenamePart1Sample))
    println(findAccWhenFixed(filenamePart1Input))
  }
}
