package aoc2022

import scala.io.Source

object Day5 {

  val filenamePart1Sample = "2022/Day5/part1-sample.txt"
  val filenamePart1Input = "2022/Day5/part1-input.txt"

  case class Rearrangement(numberToMove: Int, fromIndex: Int, toIndex: Int) {
    override def toString: String = {
      s"Move $numberToMove from ${fromIndex + 1} to ${toIndex + 1}"
    }
  }

  private val rearrangementRegex = "move (\\d+) from ([1-9]) to ([1-9])".r

  private def parseInput(
      filename: String
  ): (Vector[Vector[Char]], Vector[Rearrangement]) = {
    val lines = Source.fromResource(filename).getLines().toVector
    val sections = lines.splitAt(lines.indexOf(""))

    sections match {
      case (initialStackLines, rearrangementLines) =>
        val numStacks = initialStackLines.last.replace(" ", "").length

        val initialStacks =
          initialStackLines.init.foldLeft(
            Vector.fill(numStacks)(Vector.empty[Char])
          )((stacks, line) => {
            val crates = line
              .grouped(4)
              .map(_.trim.replace("[", "").replace("]", ""))
              .toVector
            stacks.indices
              .map(i =>
                if (i < crates.length && crates(i).nonEmpty)
                  stacks(i).appended(crates(i).head)
                else stacks(i)
              )
              .toVector
          })
        println(initialStacks)
        val instructions = rearrangementLines
          .filter(_.nonEmpty)
          .map(line => {
            val groups = rearrangementRegex.findAllMatchIn(line).toVector.head
            val numberToMove = groups.group(1).toInt
            val from = groups.group(2).toInt - 1
            val to = groups.group(3).toInt - 1
            Rearrangement(numberToMove, from, to)
          })
        (initialStacks, instructions)
    }
  }

  private def printStacks(stacks: Vector[Vector[Char]]): Unit = {
    val maxLength = stacks.map(_.length).max

    val string = (1 to maxLength)
      .map(i => {
        val index = i - 1
        stacks
          .map(s => {
            val relativeIndex = s.length - maxLength + index
            if (relativeIndex >= 0 && s.length > relativeIndex) s(relativeIndex)
            else ' '
          })
          .mkString(" ")
      })
      .mkString("\n")

    println(string)
  }

  def getTopCrates(filename: String, moveMultipleCrates: Boolean): String = {
    val finalStacks = parseInput(filenamePart1Input) match {
      case (initialStacks, rearrangements) =>
        rearrangements.foldLeft(initialStacks)((stacks, rearrangement) => {
          val fromStack = stacks(rearrangement.fromIndex)
          val toStack = stacks(rearrangement.toIndex)
          val toMove = fromStack.take(rearrangement.numberToMove)
          val toMoveFinal = if (moveMultipleCrates) toMove else toMove.reverse
          stacks
            .updated(
              rearrangement.fromIndex,
              fromStack.drop(rearrangement.numberToMove)
            )
            .updated(rearrangement.toIndex, toMoveFinal ++ toStack)
        })

    }
    finalStacks.map(_.head).mkString
  }

  def main(args: Array[String]): Unit = {
    println(getTopCrates(filenamePart1Input, moveMultipleCrates = false))
    println(getTopCrates(filenamePart1Input, moveMultipleCrates = true))
  }

}
