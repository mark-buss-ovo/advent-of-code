package aoc2022

import scala.io.Source

object Day3 {
  val filenamePart1Sample = "2022/Day3/part1-sample.txt"
  val filenamePart1Input = "2022/Day3/part1-input.txt"

  case class ItemType(code: Char, priority: Int)

  case class Compartment(items: Vector[ItemType])

  case class Rucksack(compartments: Vector[Compartment]) {
    def getDuplicate: ItemType = {
      compartments.head.items
        .filter(checkItem =>
          compartments.tail.forall(otherCompartment =>
            otherCompartment.items.contains(checkItem)
          )
        )
        .head
    }

    def allItems: Vector[ItemType] = {
      compartments.flatMap(_.items)
    }
  }

  val allLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def getRucksacks(filename: String): Vector[Rucksack] = {
    val lines = Source.fromResource(filename).getLines.toVector

    lines.foldLeft(Vector.empty[Rucksack])((rucksacks, line) => {
      if (line.isEmpty)
        rucksacks
      else {
        val halfwayPoint = line.length / 2
        val compartmentParts =
          Vector(line.substring(0, halfwayPoint), line.substring(halfwayPoint))

        val compartments = compartmentParts.map(s =>
          Compartment(
            s.map(c => ItemType(c, allLetters.indexOf(c) + 1)).toVector
          )
        )

        rucksacks.appended(Rucksack(compartments))
      }
    })
  }

  def getErrorSum(filename: String): Int = {
    val rucksacks = getRucksacks(filename)
    rucksacks.map(_.getDuplicate).map(_.priority).sum
  }

  def getErrorSumPart2(filename: String): Int = {
    getRucksacks(filename)
      .grouped(3)
      .foldLeft(0)((total, group) => {
        val commonItem = group.head.allItems
          .filter(i =>
            group.tail
              .forall(otherRucksack => otherRucksack.allItems.contains(i))
          )
          .head

        total + commonItem.priority
      })
  }

  def main(args: Array[String]): Unit = {
    println(getErrorSum(filenamePart1Input))
    println(getErrorSumPart2(filenamePart1Input))
  }

}
