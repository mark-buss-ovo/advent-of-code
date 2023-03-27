package aoc2022

import aoc2022.Day11.Operator.{Add, Multiply, fromString}

import scala.annotation.tailrec
import scala.io.Source
import scala.math.BigDecimal.RoundingMode

object Day11 {

  private val filenamePart1Sample = "2022/Day11/part1-sample.txt"
  private val filenamePart1Input = "2022/Day11/part1-input.txt"

  case class Test(divisibleBy: Long, trueMonkey: Int, falseMonkey: Int) {
    def run(item: Long): (Boolean, Int) = {
      if (item % divisibleBy == 0) (true, trueMonkey) else (false, falseMonkey)
    }
  }

  sealed trait Operator

  object Operator {
    case object Add extends Operator
    case object Multiply extends Operator

    def fromString(s: String): Operator = {
      s match {
        case "+" => Add
        case "*" => Multiply
      }
    }
  }

  sealed trait Operation {
    def calculate(old: Long): Long
  }

  case class OperationOld(operator: Operator) extends Operation {
    def calculate(old: Long): Long = {
      operator match {
        case Add      => old + old
        case Multiply => old * old
      }
    }
  }

  case class OperationNum(operator: Operator, value: Long) extends Operation {
    def calculate(old: Long): Long = operator match {
      case Add      => old + value
      case Multiply => old * value
    }
  }

  case class Monkey(
      number: Int,
      items: List[Long],
      operation: Operation,
      test: Test,
      numberOfInspectedItems: Long
  ) {
    def inspectTestAndThrow(part2: Boolean, modder: Long): List[(Long, Int)] = {
      items
        .map(item => {
          // println(s"Monkey $number inspect $item")
          val newWorryLevel = operation.calculate(item)
          // println(s"  New worry level $newWorryLevel")
          val afterRelief =
            if (part2) newWorryLevel % modder
            else Math.floor(newWorryLevel / 3).toLong

          // println(s"  After relief $afterRelief")
          val (result, monkeyToThrowTo) = test.run(afterRelief)
          // println(s"  $afterRelief ${if (result) "passes" else "fails"} test")
          // println(s"  Item ${afterRelief} thrown to ${monkeyToThrowTo}")
          (afterRelief, monkeyToThrowTo)
        })
    }
  }

  private val monkeyRegex =
    "Monkey (\\d):\\n  Starting items: ([\\d,\\s]+)\\n  Operation: new = (.*)\\n  Test: divisible by (\\d+)\\n.   If true: throw to monkey (\\d)\\n    If false: throw to monkey (\\d)".r

  def parseInput(filename: String): List[Monkey] = {
    val contents = Source.fromResource(filename).mkString
    monkeyRegex
      .findAllMatchIn(contents)
      .map(m => {
        m.subgroups match {
          case monkeyNum :: startingItems :: operation :: divisibleBy :: ifTrue :: ifFalse :: _ =>
            val op = operation.split(" ").toList match {
              case _ :: operator :: operand :: _ =>
                val or = fromString(operator)
                if (operand == "old") {
                  OperationOld(or)
                } else OperationNum(or, operand.toLong)
            }

            Monkey(
              monkeyNum.toInt,
              startingItems.split(", ").map(_.toLong).toList,
              op,
              Test(divisibleBy.toLong, ifTrue.toInt, ifFalse.toInt),
              0L
            )
        }
      })
      .toList
  }

  def doRounds(
      filename: String,
      numberOfRounds: Int,
      part2: Boolean
  ): Long = {
    val startingMonkeys = parseInput(filename)
    val modder = startingMonkeys.map(_.test.divisibleBy).product

    def doRound(roundNumber: Int, monkeys: List[Monkey]): Long = {
      if (roundNumber > numberOfRounds) {
        println(monkeys.map(m => s"${m.number} - ${m.numberOfInspectedItems}"))
        val mostActiveMonkeys =
          monkeys.sortBy(m => m.numberOfInspectedItems).reverse.take(2)

        mostActiveMonkeys match {
          case m1 :: m2 :: _ =>
            m1.numberOfInspectedItems * m2.numberOfInspectedItems
        }
      } else {
        val newMonkeys = monkeys
          .map(_.number)
          .foldLeft(monkeys)((updatedMonkeys, monkeyNumber) => {
            val monkey =
              updatedMonkeys.filter(m => m.number == monkeyNumber).head
            val itemsToThrow = monkey.inspectTestAndThrow(part2, modder)
            val uMonkeys = updatedMonkeys.map(otherMonkey => {
              if (otherMonkey.number == monkey.number) {
                monkey.copy(
                  items = List.empty,
                  numberOfInspectedItems =
                    monkey.numberOfInspectedItems + itemsToThrow.size.toLong
                )
              } else {
                val itemsForThisMonkey = itemsToThrow.collect {
                  case (i, m) if m == otherMonkey.number => i
                }
                otherMonkey.copy(items =
                  otherMonkey.items.appendedAll(itemsForThisMonkey)
                )
              }
            })

            // println(s"After Monkey ${monkey.number} - ${uMonkeys}")
            uMonkeys
          })

        if (roundNumber == 1 || roundNumber == 20 || roundNumber % 1000 == 0)
          println(s"After Round $roundNumber - ${newMonkeys
              .map(m => s"${m.number} - ${m.numberOfInspectedItems}")}")

        doRound(roundNumber + 1, newMonkeys)
      }
    }

    doRound(1, startingMonkeys)
  }

  def main(args: Array[String]): Unit = {
    println(doRounds(filenamePart1Sample, 20, false))
    println(doRounds(filenamePart1Input, 20, false))

    println(doRounds(filenamePart1Sample, 10000, true))
    println(doRounds(filenamePart1Input, 10000, true))
  }
}
