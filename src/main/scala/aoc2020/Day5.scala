package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day5 {
  val filenamePart1Sample = "2020/Day5/part1-sample.txt"
  val filenamePart1Input = "2020/Day5/part1-input.txt"

  private case class BoardingPass(row: Int, column: Int) {
    val id: Int = row * 8 + column
  }

  private def loadBoardingPasses(filename: String): List[BoardingPass] = {
    val lines = Source.fromResource(filename).getLines.toVector

    lines.foldLeft(List.empty[BoardingPass])((list, line) => {
      val dividers = line
        .map {
          case 'F' | 'L' => true
          case _         => false
        }
        .toList
        .splitAt(7)

      def getRange(isFront: Boolean, range: (Int, Int)): (Int, Int) = {
        range match {
          case (lower, upper) =>
            val diff = (upper - lower) / 2
            val midPoint = lower + diff
            if (isFront) (lower, midPoint) else (midPoint + 1, upper)
        }
      }

      @tailrec
      def findIndex(
          dividersRemaining: List[Boolean],
          range: (Int, Int)
      ): Int = {

        if (dividersRemaining.isEmpty) {
          range match {
            case (lower, _) =>
              lower
          }
        } else {
          findIndex(
            dividersRemaining.tail,
            getRange(dividersRemaining.head, range)
          )
        }
      }

      dividers match {
        case (rowDividers, colDividers) =>
          val row = findIndex(rowDividers, (0, 127))
          val col = findIndex(colDividers, (0, 7))
          list.appended(BoardingPass(row, col))
      }
    })
  }

  private def findHighestId(filename: String): Int = {
    val boardingPasses = loadBoardingPasses(filename)
    boardingPasses.maxBy(_.id).id
  }

  private def findMissingId(filename: String): Int = {
    val boardingPasses = loadBoardingPasses(filename)
    val sorted = boardingPasses.sortBy(_.id).map(_.id)

    @tailrec
    def findMissing(index: Int): Int = {
      if (sorted(index) != sorted(index + 1) - 1) {
        sorted(index) + 1
      } else findMissing(index + 1)
    }

    findMissing(0)
  }

  def main(args: Array[String]): Unit = {
    println(findHighestId(filenamePart1Sample))
    println(findHighestId(filenamePart1Input))
    println(findMissingId(filenamePart1Input))
  }
}
