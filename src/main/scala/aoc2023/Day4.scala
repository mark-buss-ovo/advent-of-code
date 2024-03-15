package aoc2023

import scala.io.Source

object Day4 {
  private val filenamePart1Sample = "2023/Day4/part1-sample.txt"
  private val filenamePart1Input = "2023/Day4/part1-input.txt"

  private case class ScratchCard(
      number: Int,
      winningNumbers: List[Int],
      myNumbers: List[Int]
  ) {
    private val winningNumberCount: Int =
      winningNumbers.intersect(myNumbers).size

    val score: Int =
      if (winningNumberCount > 0) {
        Integer.parseInt(
          s"1${"0" * (winningNumberCount - 1)}",
          2
        )
      } else 0

    val copyNumbers: List[Int] =
      (number until number + winningNumberCount).map(_ + 1).toList
  }

  private def parseCards(filename: String): List[ScratchCard] = {
    Source
      .fromResource(filename)
      .getLines()
      .map(line => {
        val parts = line.split(' ').filterNot(_ == "")
        val cardNumber = parts(1).dropRight(1).toInt
        val winningCutOff = parts.indexOf("|")
        val winningNumbers = parts.slice(2, winningCutOff).map(_.toInt).toList
        val myNumbers =
          parts.slice(winningCutOff + 1, parts.length).map(_.toInt).toList
        ScratchCard(cardNumber, winningNumbers, myNumbers)
      })
      .toList
  }

  private def getTotalScore(filename: String): Int = {
    val scratchCards = parseCards(filename)
    scratchCards.map(_.score).sum
  }

  private def getScratchCardCount(filename: String): Int = {
    val scratchCards = parseCards(filename)
    val initialCounts = scratchCards.map(_.number -> 1).toMap
    val counts = scratchCards.foldLeft(initialCounts)((cardCounts, card) => {
      val copies = cardCounts(card.number)
      val copiedCards = card.copyNumbers
      copiedCards.foldLeft(cardCounts)((wipCounts, cardNum) => {
        val currentCount = wipCounts(cardNum)
        wipCounts.updated(cardNum, currentCount + copies)
      })
    })

    counts.values.sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part 1 Sample: ${getTotalScore(filenamePart1Sample)}")
    println(s"Part 1 Input: ${getTotalScore(filenamePart1Input)}")
    println(s"Part 2 Sample: ${getScratchCardCount(filenamePart1Sample)}")
    println(s"Part 2 Input: ${getScratchCardCount(filenamePart1Input)}")
  }
}
