package aoc2020

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParMap

object Day11 {
  private val filenamePart1Sample = "2020/Day11/part1-sample.txt"
  private val filenamePart1Input = "2020/Day11/part1-input.txt"

  sealed trait PlanItem
  private case class Seat(occupied: Boolean) extends PlanItem {
    override def toString: String = if (occupied) "#" else "L"
  }
  private case object Floor extends PlanItem {
    override def toString: String = "."
  }

  case class Coordinate(x: Int, y: Int)

  private object Coordinate {
    implicit def ordering: Ordering[Coordinate] =
      (c1: Coordinate, c2: Coordinate) => {
        if (c1.y > c2.y) {
          -1
        } else if (c1.y < c2.y) {
          1
        } else if (c1.x > c2.x) {
          1
        } else if (c1.x < c2.x) {
          -1
        } else 0
      }

  }

  private def parseSeatingPlan(filename: String): Map[Coordinate, PlanItem] = {
    val lines = Source.fromResource(filename).getLines().toList
    lines.indices.foldLeft(Map.empty[Coordinate, PlanItem])((map, rowIndex) => {
      val row = lines(rowIndex)
      val rowMap = row.indices.foldLeft(Map.empty[Coordinate, PlanItem])(
        (rowMap, colIndex) => {
          val x = colIndex
          val y = lines.length - 1 - rowIndex
          val planItem = row(colIndex) match {
            case 'L' => Seat(false)
            case '.' => Floor
          }
          rowMap.updated(Coordinate(x, y), planItem)
        }
      )
      map ++ rowMap
    })
  }

  private val moveFunctions = List(
    (input: Coordinate) => input.copy(x = input.x - 1),
    (input: Coordinate) => input.copy(x = input.x - 1, y = input.y + 1),
    (input: Coordinate) => input.copy(y = input.y + 1),
    (input: Coordinate) => input.copy(x = input.x + 1, y = input.y + 1),
    (input: Coordinate) => input.copy(x = input.x + 1),
    (input: Coordinate) => input.copy(x = input.x + 1, y = input.y - 1),
    (input: Coordinate) => input.copy(y = input.y - 1),
    (input: Coordinate) => input.copy(x = input.x - 1, y = input.y - 1)
  )

  private def countOccupiedAdjacentSeats(
      coordinate: Coordinate,
      seatingPlan: ParMap[Coordinate, PlanItem]
  ): Int = {
    val allAdjacentPoints = moveFunctions.map(_(coordinate))

    val adjacentItems =
      allAdjacentPoints.map(otherSeatCoord => seatingPlan.get(otherSeatCoord))

    adjacentItems.count {
      case Some(Seat(isOccupied)) if isOccupied => true
      case _                                    => false
    }
  }

  private def countOccupiedInEyeLine(
      coordinate: Coordinate,
      seatingPlan: ParMap[Coordinate, PlanItem]
  ): Int = {
    val maxX = seatingPlan.keys.map(_.x).max
    val maxY = seatingPlan.keys.map(_.y).max

    @tailrec
    def canSeeOccupied(
        currentCoord: Coordinate,
        move: Coordinate => Coordinate
    ): Boolean = {
      val nextCoord = move(currentCoord)
      if (
        nextCoord.x < 0 || nextCoord.y < 0 || nextCoord.x > maxX || nextCoord.y > maxY
      ) false
      else {
        seatingPlan(nextCoord) match {
          case Seat(occupied) => occupied
          case _              => canSeeOccupied(nextCoord, move)
        }
      }
    }

    val count = moveFunctions.count(canSeeOccupied(coordinate, _))
    count
  }

  private def getNextSeatState(
      seatingPlan: ParMap[Coordinate, PlanItem],
      coordinate: Coordinate,
      isOccupied: Boolean,
      countOccupied: (Coordinate, ParMap[Coordinate, PlanItem]) => Int,
      threshold: Int
  ): Boolean = {
    val occupiedCount = countOccupied(coordinate, seatingPlan)

    if (isOccupied && occupiedCount >= threshold) {
      false
    } else if (!isOccupied && occupiedCount == 0) {
      true
    } else isOccupied
  }

  private def printPlan(seatingPlan: Map[Coordinate, PlanItem]): Unit = {
    val maxY = seatingPlan.keys.map(_.y).max
    val maxX = seatingPlan.keys.map(_.x).max

    (0 to maxY).reverse.foreach(y => {
      (0 to maxX).foreach(x => {
        print(seatingPlan(Coordinate(x, y)))
      })
      println()
    })
  }

  private def getOccupiedCountAfterStabilisation(
      filename: String,
      countOccupied: (Coordinate, ParMap[Coordinate, PlanItem]) => Int,
      threshold: Int
  ): Int = {
    val initialSeatingPlan = parseSeatingPlan(filename)
    // printPlan(initialSeatingPlan)
    @tailrec
    def doARound(
        previousState: ParMap[Coordinate, PlanItem],
        currentState: ParMap[Coordinate, PlanItem],
        roundNumber: Int
    ): ParMap[Coordinate, PlanItem] = {
      println(roundNumber)
      val nextState =
        currentState.par.map { case (coordinate, planItem) =>
          coordinate -> (planItem match {
            case Floor => Floor
            case Seat(occupied) =>
              Seat(
                getNextSeatState(
                  currentState,
                  coordinate,
                  occupied,
                  countOccupied,
                  threshold
                )
              )
          })
        }.toMap

      // println("====")
      // printPlan(nextState)

      if (nextState == previousState) {
        nextState
      } else {
        doARound(currentState, nextState, roundNumber + 1)
      }
    }

    val stableState =
      doARound(initialSeatingPlan.par, initialSeatingPlan.par, 1)
    stableState.values.count {
      case Seat(occupied) if occupied => true
      case _                          => false
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      getOccupiedCountAfterStabilisation(
        filenamePart1Sample,
        countOccupiedAdjacentSeats,
        4
      )
    )
    println(
      getOccupiedCountAfterStabilisation(
        filenamePart1Input,
        countOccupiedAdjacentSeats,
        4
      )
    )
    println(
      getOccupiedCountAfterStabilisation(
        filenamePart1Sample,
        countOccupiedInEyeLine,
        5
      )
    )
    println(
      getOccupiedCountAfterStabilisation(
        filenamePart1Input,
        countOccupiedInEyeLine,
        5
      )
    )
  }
}
