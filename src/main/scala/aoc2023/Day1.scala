package aoc2023

import scala.io.Source

object Day1 {
  private val filenamePart1Sample = "2023/Day1/part1-sample.txt"
  private val filenamePart1Input = "2023/Day1/part1-input.txt"
  private val filenamePart2Sample = "2023/Day1/part2-sample.txt"

  private def getCalibrationValues(filename: String): List[Int] = {
    Source
      .fromResource(filename)
      .getLines
      .map(line => {
        val numberChars = line.replaceAll("[^0-9]", "")
        s"${numberChars.head}${numberChars.last}".toInt
      })
      .toList
  }

  private def getCalibrationTotal(filename: String): Int = {
    getCalibrationValues(filename).sum
  }

  private val digits: List[String] =
    List("1", "2", "3", "4", "5", "6", "7", "8", "9")
  private val words: List[String] =
    List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  private val all: List[String] = digits ++ words

  private case class Match(
      number: String,
      firstIndex: Int,
      lastIndex: Int
  )

  private def getInt(number: String): Int = {
    if (digits.contains(number)) number.toInt else words.indexOf(number) + 1
  }

  private def getCalibrationValuesPart2(filename: String): List[Int] = {
    Source
      .fromResource(filename)
      .getLines
      .map(line => {
        val matchesWithIndex = all
          .collect(s => {
            val first = line.indexOf(s)
            val last = line.lastIndexOf(s)
            Option.when(first >= 0)(Match(s, first, last))
          })
          .flatten
        val firstHit = matchesWithIndex.minBy(_.firstIndex).number
        val lastHit = matchesWithIndex.maxBy(_.lastIndex).number
        s"${getInt(firstHit)}${getInt(lastHit)}".toInt
      })
      .toList
  }

  private def getCalibrationTotalPart2(filename: String): Int = {
    val values = getCalibrationValuesPart2(filename)
    values.sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part 1 Sample: ${getCalibrationTotal(filenamePart1Sample)}")
    println(s"Part 1 Input: ${getCalibrationTotal(filenamePart1Input)}")
    println(s"Part 2 Sample: ${getCalibrationTotalPart2(filenamePart2Sample)}")
    println(s"Part 2 Input: ${getCalibrationTotalPart2(filenamePart1Input)}")
  }
}
