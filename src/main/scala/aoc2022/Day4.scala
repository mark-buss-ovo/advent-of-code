package aoc2022

import scala.io.Source

object Day4 {
  val filenamePart1Sample = "2022/Day4/part1-sample.txt"
  val filenamePart1Input = "2022/Day4/part1-input.txt"

  def getRangePairs(filename: String): Vector[(Range, Range)] = {
    val lines = Source.fromResource(filename).getLines.toVector

    lines.foldLeft(Vector.empty[(Range, Range)])((rangePairs, line) => {
      if (line.isEmpty)
        rangePairs
      else {
        val ranges =
          line
            .split(',')
            .map(_.split('-'))
            .map(n => Range.inclusive(n.head.toInt, n.last.toInt))
        rangePairs.appended((ranges.head, ranges.last))
      }
    })
  }

  def getCountFullyContained(filename: String): Int = {
    getRangePairs(filename).count { case (range1, range2) =>
      (range1.min <= range2.min && range1.max >= range2.max) || (range2.min <= range1.min && range2.max >= range1.max)
    }
  }

  def getCountOverlaps(filename: String): Int = getRangePairs(filename).count {
    case (range1, range2) =>
      range1.exists(r1 => range2.contains(r1))
  }

  def main(args: Array[String]): Unit = {
    println(getCountFullyContained(filenamePart1Input))
    println(getCountOverlaps(filenamePart1Input))
  }

}
