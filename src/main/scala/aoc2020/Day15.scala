package aoc2020

import scala.annotation.tailrec

object Day15 {

  private val part1Sample1 = "0,3,6"
  private val part1Sample2 = "1,3,2"
  private val part1Sample3 = "2,1,3"
  private val part1Sample4 = "1,2,3"
  private val part1Sample5 = "2,3,1"
  private val part1Sample6 = "3,2,1"
  private val part1Sample7 = "3,1,2"
  private val part1Input = "0,13,1,16,6,17"

  private def getNumberSpoken(input: String): Int = {
    val startTime = System.nanoTime()
    val startingNumbers = input.split(',').map(_.toInt)

    val initialTurns = startingNumbers.zipWithIndex.map {
      case (startingNumber, index) => (index + 1, startingNumber)
    }.toMap

    @tailrec
    def takeTurn(
        nextTurnNumber: Int,
        turns: Map[Int, Int],
        lastSpoken: Int
    ): Int = {
      val hits = turns.filter { case (_, spoken) => spoken == lastSpoken }
      val mostRecent = hits.toList
        .sortBy { case (turnNumber, _) =>
          turnNumber
        }
        .reverse
        .take(2)

      val nextSpoken = if (mostRecent.size == 2) {
        val turns = mostRecent.map { case (turnNumber, _) => turnNumber }
        turns.head - turns.last
      } else {
        0
      }

      if (nextTurnNumber == 2020) nextSpoken
      else
        takeTurn(
          nextTurnNumber + 1,
          turns.updated(nextTurnNumber, nextSpoken),
          nextSpoken
        )
    }

    val result =
      takeTurn(initialTurns.size + 1, initialTurns, startingNumbers.last)
    val duration = (System.nanoTime() - startTime) / 1e9d
    println(s"Took $duration seconds")
    result
  }

  case class Record(previous: Option[Long], last: Long) {
    def updated(newTurnNumber: Long): Record =
      copy(previous = Some(last), last = newTurnNumber)
  }

  private def getNumberSpokenPart2(input: String): Long = {
    val startTime = System.nanoTime()
    val startingNumbers = input.split(',').map(_.toLong)

    val startingRecords = startingNumbers.zipWithIndex.map {
      case (startingNumber, turnNumber) =>
        (startingNumber, Record(None, turnNumber + 1))
    }.toMap

    @tailrec
    def takeTurn(
        nextTurnNumber: Long,
        records: Map[Long, Record],
        lastSpoken: Long
    ): Long = {
      val nextSpoken = records(lastSpoken) match {
        case Record(Some(previous), last) => last - previous
        case _                            => 0
      }

      if (nextTurnNumber == 30000000) nextSpoken
      else {
        val updatedRecord = records
          .get(nextSpoken)
          .map(r => r.updated(nextTurnNumber))
          .getOrElse(Record(None, nextTurnNumber))
        takeTurn(
          nextTurnNumber + 1,
          records.updated(nextSpoken, updatedRecord),
          nextSpoken
        )
      }

    }

    val result =
      takeTurn(startingRecords.size + 1, startingRecords, startingNumbers.last)
    val duration = (System.nanoTime() - startTime) / 1e9d
    println(s"Took $duration seconds")
    result
  }

  def main(args: Array[String]): Unit = {
    println(getNumberSpoken(part1Sample1))
    println(getNumberSpoken(part1Sample2))
    println(getNumberSpoken(part1Sample3))
    println(getNumberSpoken(part1Sample4))
    println(getNumberSpoken(part1Sample5))
    println(getNumberSpoken(part1Sample6))
    println(getNumberSpoken(part1Sample7))
    println(getNumberSpoken(part1Input))

    println(getNumberSpokenPart2(part1Sample1))
    println(getNumberSpokenPart2(part1Sample2))
    println(getNumberSpokenPart2(part1Sample3))
    println(getNumberSpokenPart2(part1Sample4))
    println(getNumberSpokenPart2(part1Sample5))
    println(getNumberSpokenPart2(part1Sample6))
    println(getNumberSpokenPart2(part1Sample7))
    println(getNumberSpokenPart2(part1Input))
  }

}
