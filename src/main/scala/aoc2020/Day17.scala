package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day17 {

  private val filenamePart1Sample = "2020/Day17/part1-sample.txt"
  private val filenamePart1Input = "2020/Day17/part1-input.txt"

  private case class Coordinate(x: Int, y: Int, z: Int) {
    def isActiveAfterCycle(
        isActive: Boolean,
        activeCoords: Vector[Coordinate]
    ): Boolean = {

      val neighbourCoords = (for {
        x <- x - 1 to x + 1
        y <- y - 1 to y + 1
        z <- z - 1 to z + 1
      } yield Coordinate(x, y, z)).filterNot(_ == this)

      val countActiveNeighbours =
        neighbourCoords.count(activeCoords.contains(_))

      if (isActive) {
        countActiveNeighbours == 2 || countActiveNeighbours == 3
      } else countActiveNeighbours == 3
    }
  }

  private def parseInitialState(filename: String): Vector[Coordinate] = {
    val lines = Source.fromResource(filename).getLines().toList

    lines.indices.reverse.foldLeft(Vector.empty[Coordinate])(
      (activeCoords, y) => {
        val line = lines(lines.size - y - 1)
        val activeXs = line.indices.filter(line(_) == '#')
        activeCoords.appendedAll(activeXs.map(Coordinate(_, y, 0)))
      }
    )
  }

  private def getActiveCount(filename: String): Int = {
    val initialState = parseInitialState(filename)

    @tailrec
    def cycle(activeCoords: Vector[Coordinate], cycleNumber: Int): Int = {

      val minX = activeCoords.map(_.x).min - 1
      val minY = activeCoords.map(_.y).min - 1
      val minZ = activeCoords.map(_.z).min - 1

      val maxX = activeCoords.map(_.x).max + 1
      val maxY = activeCoords.map(_.y).max + 1
      val maxZ = activeCoords.map(_.z).max + 1

      val allCoords = (for {
        x <- minX to maxX
        y <- minY to maxY
        z <- minZ to maxZ
      } yield Coordinate(x, y, z)).toVector

      val updatedCoords = allCoords.filter(coord => {
        val isActive = activeCoords.contains(coord)
        coord.isActiveAfterCycle(isActive, activeCoords)
      })

      if (cycleNumber + 1 > 6) {
        updatedCoords.size
      } else {
        cycle(updatedCoords, cycleNumber + 1)
      }
    }

    cycle(initialState, 1)
  }

  private case class CoordinateV2(values: Map[Char, Int]) {
    def isActiveAfterCycle(activeCoords: Vector[CoordinateV2]): Boolean = {

      val neighbourCoords = getCombinationCoords(values.map { case (a, v) =>
        getAxisValues(a, v - 1 to v + 1)
      }.toList).filterNot(c => c == this)

      val countActiveNeighbours =
        neighbourCoords.count(activeCoords.contains(_))

      if (activeCoords.contains(this)) {
        countActiveNeighbours == 2 || countActiveNeighbours == 3
      } else countActiveNeighbours == 3
    }
  }

  private def combinationList(
      ls: List[List[(Char, Int)]]
  ): List[List[(Char, Int)]] = ls match {
    case Nil => Nil :: Nil
    case head :: tail =>
      val rec = combinationList(tail)
      rec.flatMap(r => head.map(t => t :: r))
  }

  private def getCombinationCoords(
      startingList: List[List[(Char, Int)]]
  ): List[CoordinateV2] = {
    combinationList(startingList).map(a => CoordinateV2(a.toMap))
  }

  private def getAxisValues(axisName: Char, range: Range): List[(Char, Int)] = {
    range.toList.map(value => (axisName, value))
  }

  private def parseInitialStateV2(
      filename: String,
      otherAxis: List[Char]
  ): Vector[CoordinateV2] = {
    val lines = Source.fromResource(filename).getLines().toList

    lines.indices.reverse.foldLeft(Vector.empty[CoordinateV2])(
      (activeCoords, y) => {
        val line = lines(lines.size - y - 1)
        val activeXs = line.indices.filter(line(_) == '#')
        activeCoords.appendedAll(
          activeXs.map(x =>
            CoordinateV2(Map('x' -> x, 'y' -> y) ++ otherAxis.map(c => c -> 0))
          )
        )
      }
    )
  }

  private def getActiveCountV2(
      filename: String,
      additionalAxis: List[Char]
  ): Int = {
    val initialState = parseInitialStateV2(filename, additionalAxis)

    @tailrec
    def cycle(activeCoords: Vector[CoordinateV2], cycleNumber: Int): Int = {

      val axisRanges = activeCoords.head.values.keys
        .map(c =>
          getAxisValues(
            c,
            activeCoords.map(_.values(c)).min - 1 to activeCoords
              .map(_.values(c))
              .max + 1
          )
        )
        .toList

      val allCoords = getCombinationCoords(axisRanges).toVector

      val updatedCoords = allCoords.filter(coord => {
        coord.isActiveAfterCycle(activeCoords)
      })

      // println(s"Cycle: $cycleNumber, active count: ${updatedCoords.size}")

      //      (minZ to maxZ).foreach(z =>
      //        println(s"z: $z, active count: ${updatedCoords.count(_.z == z)}")
      //      )

      if (cycleNumber + 1 > 6) {
        updatedCoords.size
      } else {
        cycle(updatedCoords, cycleNumber + 1)
      }
    }

    cycle(initialState, 1)
  }

  def main(args: Array[String]): Unit = {
    println(getActiveCount(filenamePart1Sample))
    println(getActiveCount(filenamePart1Input))

    println(getActiveCountV2(filenamePart1Sample, List('z')))
    println(getActiveCountV2(filenamePart1Input, List('z')))

    println(getActiveCountV2(filenamePart1Sample, List('z', 'w')))
    println(getActiveCountV2(filenamePart1Input, List('z', 'w')))
  }
}
