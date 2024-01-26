package aoc2023

import scala.io.Source

object Day2 {
  private val filenamePart1Sample = "2023/Day2/part1-sample.txt"
  private val filenamePart1Input = "2023/Day2/part1-input.txt"

  private case class Reveal(cubeCounts: Map[String, Int])

  private case class Game(id: Int, reveals: List[Reveal]) {
    def power: Int = {
      val uniqueColours = reveals.flatMap(_.cubeCounts.keys).distinct

      uniqueColours
        .map(colour => reveals.flatMap(_.cubeCounts.get(colour)).max)
        .product
    }
  }

  private def parseGames(filename: String): List[Game] = {
    Source.fromResource(filename).getLines().map {
      case s"Game $id: $restOfLine" =>
        val reveals = restOfLine
          .split(";")
          .map(r =>
            Reveal(
              r.split(",")
                .map(_.trim)
                .map { case s"$count $colour" =>
                  colour -> count.toInt
                }
                .toMap
            )
          )
          .toList
        Game(id.toInt, reveals)
    }
  }.toList

  private def totalPossible(
      filename: String,
      bagContents: Map[String, Int]
  ): Int = {
    val games = parseGames(filename)
    val possibleGames = games.filter(g => {
      bagContents.forall { case (checkColour, checkCount) =>
        g.reveals.forall(reveal => {
          reveal.cubeCounts
            .get(checkColour)
            .forall(_ <= checkCount)
        })
      }
    })

    possibleGames.map(_.id).sum
  }

  private def sumPowers(filename: String): Int = {
    val games = parseGames(filename)
    games.map(_.power).sum
  }

  def main(args: Array[String]): Unit = {
    val bagContents = Map(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )
    println(
      s"Part 1 Sample: ${totalPossible(filenamePart1Sample, bagContents)}"
    )
    println(
      s"Part 1 Input: ${totalPossible(filenamePart1Input, bagContents)}"
    )
    println(      s"Part 2 Sample: ${sumPowers(filenamePart1Sample)}"    )
    println(      s"Part 2 Input: ${sumPowers(filenamePart1Input)}"    )
  }
}
