package aoc2023

import scala.io.Source

object Day3 {
  private val filenamePart1Sample = "2023/Day3/part1-sample.txt"
  private val filenamePart1Input = "2023/Day3/part1-input.txt"

  private case class Coordinate(x: Int, y: Int)

  private case class PartNumber(value: Int, y: Int, x: Int) {
    def adjacentTo(coordinate: Coordinate): Boolean =
      Math.abs(coordinate.y - y) <= 1 &&
        x - 1 <= coordinate.x && coordinate.x <= x + value.toString.length
  }

  private case class Symbol(value: Char, coordinate: Coordinate)

  private case class ParseState(
      currentPartInfo: Option[(String, Int)],
      partNumbers: List[PartNumber],
      symbols: List[Symbol]
  )

  private case class Domain(parts: List[PartNumber], symbols: List[Symbol])

  private def parseInput(filename: String): Domain = {
    val initialState =
      ParseState(None, List.empty, List.empty)
    val lines = Source
      .fromResource(filename)
      .getLines()
      .toList
    val finalState = lines.indices
      .foldLeft(initialState)((state, rowIndex) => {
        val line = lines(rowIndex)
        line.indices.foldLeft(state.copy(currentPartInfo = None))(
          (lineState, colIndex) => {
            val char = line(colIndex)
            val newPartInfo = Option.when(char.isDigit)(
              lineState.currentPartInfo match {
                case None            => (char.toString, colIndex)
                case Some((part, x)) => (part + char, x)
              }
            )

            val newPartNumbers =
              if (!char.isDigit || colIndex == line.length - 1) {
                val toMatch =
                  if (!char.isDigit) lineState.currentPartInfo else newPartInfo
                toMatch match {
                  case Some((part, x)) =>
                    lineState.partNumbers.appended(
                      PartNumber(part.toInt, rowIndex, x)
                    )
                  case None => lineState.partNumbers
                }
              } else lineState.partNumbers

            val newSymbols = if (!char.isDigit && char != '.') {
              lineState.symbols.appended(
                Symbol(char, Coordinate(colIndex, rowIndex))
              )
            } else lineState.symbols

            newPartInfo match {
              case Some((newPart, newPartX)) =>
                ParseState(Some(newPart, newPartX), newPartNumbers, newSymbols)
              case None => ParseState(None, newPartNumbers, newSymbols)
            }

          }
        )
      })

    Domain(finalState.partNumbers, finalState.symbols)
  }

  private def getPartsSum(filename: String): Int = {
    val domain = parseInput(filename)

    domain.symbols
      .flatMap(symbol => {
        val adjacentNumbers =
          domain.parts.filter(p => p.adjacentTo(symbol.coordinate))
        adjacentNumbers
      })
      .distinct
      .map(_.value)
      .sum
  }

  private def getSumGearRatios(filename: String): Int = {
    val domain = parseInput(filename)

    domain.symbols
      .filter(s => s.value == '*')
      .map(s => domain.parts.filter(_.adjacentTo(s.coordinate)))
      .filter(_.size == 2)
      .map(_.map(_.value))
      .map(_.product)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part 1 Sample: ${getPartsSum(filenamePart1Sample)}")
    println(s"Part 1 Input: ${getPartsSum(filenamePart1Input)}")
    println(s"Part 2 Sample: ${getSumGearRatios(filenamePart1Sample)}")
    println(s"Part 2 Input: ${getSumGearRatios(filenamePart1Input)}")
  }
}
