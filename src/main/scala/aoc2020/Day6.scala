package aoc2020

import scala.io.Source

object Day6 {
  val filenamePart1Sample = "2020/Day6/part1-sample.txt"
  val filenamePart1Input = "2020/Day6/part1-input.txt"

  def getAnyoneCountSum(filename: String): Int = {
    val lines = Source.fromResource(filename).getLines()

    val groupSets = lines.foldLeft(List(Set.empty[Char]))((list, line) => {
      if (line.isEmpty) {
        list.appended(Set.empty[Char])
      } else {
        list.updated(list.length - 1, list.last ++ line)
      }
    })

    groupSets.map(_.size).sum

  }

  def getEveryoneCountSum(filename: String): Int = {
    val lines = Source.fromResource(filename).getLines()

    val groups = lines.foldLeft(List(List.empty[String]))((list, line) => {
      if (line.isEmpty) {
        list.appended(List.empty[String])
      } else {
        list.updated(list.length - 1, list.last.appended(line))
      }
    })

    groups
      .map(g => {
        val uniqueChars = g.flatten.toSet
        uniqueChars.count(c => g.forall(s => s.contains(c)))
      })
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(getAnyoneCountSum(filenamePart1Sample))
    println(getAnyoneCountSum(filenamePart1Input))

    println(getEveryoneCountSum(filenamePart1Sample))
    println(getEveryoneCountSum(filenamePart1Input))
  }

}
