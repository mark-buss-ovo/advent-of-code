package aoc2022

import scala.annotation.tailrec
import scala.io.Source

object Day10 {

  private val filenamePart1Sample = "2022/Day10/part1-sample.txt"
  private val filenamePart1Input = "2022/Day10/part1-input.txt"

  type Pixels = List[List[Boolean]]

  sealed trait Instruction {
    def numberOfCycles: Int
  }

  case object NoOp extends Instruction {
    def numberOfCycles: Int = 1
  }

  case class Add(value: Int) extends Instruction {
    def numberOfCycles: Int = 2
  }

  def parseInput(filename: String): List[Instruction] = {
    val lines = Source.fromResource(filename).getLines()
    lines.map {
      case "noop" => NoOp
      case l      => Add(l.split(' ')(1).toInt)
    }.toList
  }

  private def sumRelevantFrequencies(filename: String): Int = {
    val instructions = parseInput(filename)
    val relevantCycles = List(20, 60, 100, 140, 180, 220)

    @tailrec
    def cycle(
        cycleNumber: Int,
        instructionIndex: Int,
        x: Int,
        nextRelevantCycleIndex: Int,
        sumSoFar: Int
    ): Int = {

      if (nextRelevantCycleIndex > relevantCycles.length - 1) {
        sumSoFar
      } else {
        val instruction = instructions(instructionIndex)
        val nextRelevantCycle = relevantCycles(nextRelevantCycleIndex)
        val (newSum, newRelevantCycleIndex) = {
          if (
            (cycleNumber to cycleNumber + instruction.numberOfCycles)
              .contains(nextRelevantCycle)
          ) {
            (
              sumSoFar + (x * nextRelevantCycle),
              nextRelevantCycleIndex + 1
            )
          } else (sumSoFar, nextRelevantCycleIndex)
        }

        val newX = instruction match {
          case Add(value) => x + value
          case _          => x
        }

        cycle(
          cycleNumber + instruction.numberOfCycles,
          getNextInstructionIndex(instructions, instructionIndex),
          newX,
          newRelevantCycleIndex,
          newSum
        )
      }
    }

    cycle(0, 0, 1, 0, 0)
  }

  private def getNextInstructionIndex(
      instructions: List[Instruction],
      instructionIndex: Int
  ) = {
    if (instructionIndex + 1 > instructions.length - 1) 0
    else instructionIndex + 1
  }

  private def drawPixels(filename: String): Unit = {
    val instructions = parseInput(filename)

    val startPixels = List.fill(6)(List.fill(40)(false))

    def draw(pixels: Pixels): Unit = {
      pixels.foreach(row =>
        println(row.map(pixel => if (pixel) '#' else '.').mkString)
      )
    }

    @tailrec
    def cycle(
        spritePosition: Int,
        cycleNumber: Int,
        pixels: Pixels,
        instructionIndex: Int
    ): Unit = {
      if (cycleNumber == 241)
        draw(pixels)
      else {

        val instruction = instructions(instructionIndex)

        @tailrec
        def drawForInstruction(
            processedCycles: Int,
            numberToProcess: Int,
            wipPixels: Pixels
        ): Pixels = {
          if (processedCycles == numberToProcess) wipPixels
          else {
            val row = (cycleNumber - 1 + processedCycles) / 40
            val column = (cycleNumber - 1 + processedCycles) % 40
            val lit =
              (spritePosition - 1 to spritePosition + 1).contains(column)
            val updatedPixels =
              wipPixels.updated(row, wipPixels(row).updated(column, lit))
            drawForInstruction(
              processedCycles + 1,
              numberToProcess,
              updatedPixels
            )
          }
        }

        val updatedPixels =
          drawForInstruction(0, instruction.numberOfCycles, pixels)

        val newSpritePosition = instruction match {
          case Add(value) => spritePosition + value
          case _          => spritePosition
        }

        cycle(
          newSpritePosition,
          cycleNumber + instruction.numberOfCycles,
          updatedPixels,
          getNextInstructionIndex(instructions, instructionIndex)
        )
      }
    }

    cycle(1, 1, startPixels, 0)
  }

  def main(args: Array[String]): Unit = {
    println(sumRelevantFrequencies(filenamePart1Sample))
    println(sumRelevantFrequencies(filenamePart1Input))

    drawPixels(filenamePart1Sample)
    drawPixels(filenamePart1Input)

  }
}
