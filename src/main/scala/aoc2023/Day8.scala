package aoc2023

import scala.annotation.tailrec
import scala.io.Source

object Day8 {
  private val filenamePart1Sample1 = "2023/Day8/part1-sample1.txt"
  private val filenamePart1Sample2 = "2023/Day8/part1-sample2.txt"
  private val filenamePart1Input = "2023/Day8/part1-input.txt"
  private val filenamePart2Sample = "2023/Day8/part2-sample.txt"

  private case class Node(left: String, right: String) {
    def next(instruction: Char): String =
      if (instruction == 'L') left else right
  }

  private case class Domain(instructions: String, nodes: Map[String, Node]) {
    def followInstructions(): Int = {
      @tailrec
      def followOne(
          nodeId: String,
          instructionIndex: Int,
          stepsSoFar: Int
      ): Int = {
        val instruction = instructions(instructionIndex)
        val nextNodeId = nodes(nodeId).next(instruction)
        if (nextNodeId == "ZZZ") {
          stepsSoFar + 1
        } else {
          followOne(
            nextNodeId,
            getNextInstructionIndex(instructionIndex),
            stepsSoFar + 1
          )
        }
      }

      followOne("AAA", 0, 0)
    }

    def followInstructionsGhostMode(): Int = {
      val allStartNodes = nodes.keys.filter(_.endsWith("A")).toList

      def followOne(
          currentNodeIds: List[String],
          instructionIndex: Int,
          stepsSoFar: Int
      ): Int = {
        val currentNodes = currentNodeIds.map(nodes(_))
        val instruction = instructions(instructionIndex)
        val nextNodeIds = currentNodes.map(_.next(instruction))

        if (nextNodeIds.forall(_.endsWith("Z"))) {
          stepsSoFar + 1
        } else {
          followOne(
            nextNodeIds,
            getNextInstructionIndex(instructionIndex),
            stepsSoFar + 1
          )
        }
      }
      followOne(allStartNodes, 0, 0)
    }

    private def getNextInstructionIndex(instructionIndex: Int) = {
      if (instructionIndex == instructions.length - 1) 0
      else instructionIndex + 1
    }

    def followInstructionsGhostModeAttempt2(): Int = {
      val allStartNodes = nodes.keys.filter(_.endsWith("A"))

      val maxSteps = 1000000

      @tailrec
      def getPossibleEndSteps(
          nodeId: String,
          instructionIndex: Int,
          stepsSoFar: Int,
          endSteps: List[Int]
      ): List[Int] = {
        val node = nodes(nodeId)
        val nextNodeId = node.next(instructions(instructionIndex))
        val nextEndSteps =
          if (nextNodeId.endsWith("Z")) endSteps.appended(stepsSoFar + 1)
          else endSteps
        if ((stepsSoFar + 1) == maxSteps) nextEndSteps
        else
          getPossibleEndSteps(
            nextNodeId,
            getNextInstructionIndex(instructionIndex),
            stepsSoFar + 1,
            nextEndSteps
          )
      }

      val paths =
        allStartNodes.map(getPossibleEndSteps(_, 0, 0, List.empty))

      paths.head
        .find(step => paths.tail.forall(_.contains(step)))
        .getOrElse(-100)
    }
  }

  private def parseDomain(filename: String): Domain = {
    val lines = Source.fromResource(filename).getLines().toList

    val instructions = lines.head

    val nodes = lines
      .drop(2)
      .map { case s"$id = ($left, $right)" =>
        (id, Node(left, right))
      }
      .toMap

    Domain(instructions, nodes)
  }

  def main(args: Array[String]): Unit = {
    println(
      s"Part 1 - Sample 1: ${parseDomain(filenamePart1Sample1).followInstructions()}"
    )
    println(
      s"Part 1 - Sample 2: ${parseDomain(filenamePart1Sample2).followInstructions()}"
    )
    println(
      s"Part 1 - Input: ${parseDomain(filenamePart1Input).followInstructions()}"
    )
    println(
      s"Part 2 - Sample: ${parseDomain(filenamePart2Sample).followInstructionsGhostModeAttempt2()}"
    )
    println(
      s"Part 2 - Input: ${parseDomain(filenamePart1Input).followInstructionsGhostModeAttempt2()}"
    )
  }
}
