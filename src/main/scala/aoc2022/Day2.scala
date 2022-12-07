package aoc2022

import scala.io.Source

object Day2 {
  val filenamePart1Sample = "2022/Day2/part1-sample.txt"
  val filenamePart1Input = "2022/Day2/part1-input.txt"

  sealed trait Play {
    val score: Int
    val winsAgainst: Play
    val losesTo: Play
  }

  case object Rock extends Play {
    val score: Int = 1

    val winsAgainst: Play = Scissors
    val losesTo: Play = Paper
  }

  case object Paper extends Play {
    val score: Int = 2

    val winsAgainst: Play = Rock
    val losesTo: Play = Scissors
  }

  case object Scissors extends Play {
    val score: Int = 3

    val winsAgainst: Play = Paper
    val losesTo: Play = Rock
  }

  case class Round(opponent: Play, self: Play) {
    def score: Int = {
      if (self == opponent)
        self.score + 3
      else if (self.winsAgainst == opponent)
        self.score + 6
      else self.score
    }
  }

  def getRounds(filename: String): Vector[Round] = {
    val lines = Source.fromResource(filename).getLines.toVector

    lines.foldLeft(Vector.empty[Round])((rounds, line) => {
      val parts = line.split(' ')

      val opponent = parts.head.head match {
        case 'A' => Rock
        case 'B' => Paper
        case 'C' => Scissors
      }

      val self = parts.last.head match {
        case 'X' => Rock
        case 'Y' => Paper
        case 'Z' => Scissors
      }

      rounds.appended(Round(opponent, self))
    })
  }

  def getScore(rounds: Vector[Round]): Int = {
    rounds.map(_.score).sum
  }

  sealed trait Outcome
  object Outcome {
    case object Lose extends Outcome
    case object Draw extends Outcome
    case object Win extends Outcome
  }

  def getRoundsPart2(filename: String): Vector[Round] = {
    val lines = Source.fromResource(filename).getLines.toVector

    lines.foldLeft(Vector.empty[Round])((rounds, line) => {
      val parts = line.split(' ')

      val opponent = parts.head.head match {
        case 'A' => Rock
        case 'B' => Paper
        case 'C' => Scissors
      }

      val self = parts.last.head match {
        case 'X' => opponent.winsAgainst
        case 'Y' => opponent
        case 'Z' => opponent.losesTo
      }

      rounds.appended(Round(opponent, self))
    })
  }

  def main(args: Array[String]): Unit = {
    println(getScore(getRounds(filenamePart1Input)))
    println(getScore(getRoundsPart2(filenamePart1Input)))
  }

}
