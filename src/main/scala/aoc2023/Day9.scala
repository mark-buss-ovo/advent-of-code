package aoc2023

import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  private val filenamePart1Sample = "2023/Day9/part1-sample.txt"
  private val filenamePart1Input = "2023/Day9/part1-input.txt"

  case class History(values: List[Int]) {
    @tailrec
    private def buildSequences(
        sequencesSoFar: List[List[Int]]
    ): List[List[Int]] = {
      val prevSequence = sequencesSoFar.last
      val nextSequence =
        prevSequence.zip(prevSequence.tail).map { case (a, b) =>
          b - a
        }

      if (nextSequence.exists(_ != 0)) {
        buildSequences(sequencesSoFar.appended(nextSequence))
      } else {
        sequencesSoFar.appended(nextSequence)
      }
    }

    def getNextValue: Int = {

      val bottomUpSequences = buildSequences(List(values)).reverse

      @tailrec
      def findNextValues(
          remainingSequences: List[List[Int]],
          prevMissingValue: Int
      ): Int = {
        val current = remainingSequences.head
        val nextMissingValue = current.last + prevMissingValue

        if (remainingSequences.size == 1) {
          nextMissingValue
        } else {
          findNextValues(remainingSequences.tail, nextMissingValue)
        }
      }

      findNextValues(bottomUpSequences.tail, 0)
    }

    def getPreviousValue: Int = {
      val bottomUpSequences = buildSequences(List(values)).reverse

      @tailrec
      def findPreviousValues(
          remainingSequences: List[List[Int]],
          prevMissingValue: Int
      ): Int = {
        val current = remainingSequences.head
        val nextMissingValue = current.head - prevMissingValue

        if (remainingSequences.size == 1) {
          nextMissingValue
        } else {
          findPreviousValues(remainingSequences.tail, nextMissingValue)
        }
      }

      findPreviousValues(bottomUpSequences.tail, 0)
    }

  }

  case class Report(histories: List[History]) {
    def getSumOfNextValues: Int = histories.map(_.getNextValue).sum
    def getSumOfPreviousValues: Int = histories.map(_.getPreviousValue).sum
  }

  private def parseReport(filename: String): Report = {
    val histories = Source
      .fromResource(filename)
      .getLines()
      .map(line => {
        History(line.split(' ').map(_.toInt).toList)
      })
      .toList
    Report(histories)
  }

  def main(args: Array[String]): Unit = {
    println(
      s"Part 1 - Sample: ${parseReport(filenamePart1Sample).getSumOfNextValues}"
    )
    println(
      s"Part 1 - Input: ${parseReport(filenamePart1Input).getSumOfNextValues}"
    )
    println (
      s"Part 2 - Sample: ${parseReport(filenamePart1Sample).getSumOfPreviousValues}"
      )
    println(
      s"Part 2 - Input: ${parseReport(filenamePart1Input).getSumOfPreviousValues}"
    )
  }
}
