package aoc2022

import aoc2022.Day9.Direction._

import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  private val filenamePart1Sample = "2022/Day9/part1-sample.txt"
  private val filenamePart1Input = "2022/Day9/part1-input.txt"

  sealed trait Direction

  object Direction {
    case object Left extends Direction
    case object Right extends Direction
    case object Up extends Direction
    case object Down extends Direction

    def fromChar(c: Char): Direction = {
      c match {
        case 'L' => Left
        case 'R' => Right
        case 'U' => Up
        case 'D' => Down
      }
    }
  }

  case class Motion(direction: Direction, number: Int)

  case class Point(x: Int, y: Int) {
    def moveOne(direction: Direction): Point = {
      direction match {
        case Left  => copy(x - 1, y)
        case Right => copy(x + 1, y)
        case Up    => copy(x, y - 1)
        case Down  => copy(x, y + 1)
      }
    }

    def moveCloser(otherPoint: Point): Point = {

      val xDifference = Math.abs(otherPoint.x - x)
      val yDifference = Math.abs(otherPoint.y - y)

      if (xDifference <= 1 && yDifference <= 1)
        this
      else
        Point(x + otherPoint.x.compareTo(x), y + otherPoint.y.compareTo(y))
    }
  }

  object Point {
    def startPosition: Point = Point(0, 0)
  }

  def parseInput(filename: String): List[Motion] = {
    val lines = Source.fromResource(filename).getLines()
    lines
      .map(l =>
        l.split(' ') match {
          case Array(c, num) => Motion(Direction.fromChar(c.head), num.toInt)
        }
      )
      .toList
  }

  def countVisitedPositions(filename: String, numberOfKnots: Int): Int = {
    val motions = parseInput(filename)

    @tailrec
    def goThroughTheMotions(
        index: Int,
        knots: Vector[Point],
        visited: Set[Point]
    ): Int = {
      if (index > motions.length - 1) {
        visited.size
      } else {
        val motion = motions(index)

        @tailrec
        def runMotion(
            numberToGo: Int,
            wipKnots: Vector[Point],
            wipVisited: Set[Point]
        ): (Vector[Point], Set[Point]) = {
          if (numberToGo == 0)
            (wipKnots, wipVisited)
          else {
            val newWipHead = wipKnots.head.moveOne(motion.direction)
            val newWipKnots =
              wipKnots.tail.foldLeft(Vector(newWipHead))(
                (movedKnots, currentKnot) => {
                  movedKnots.appended(currentKnot.moveCloser(movedKnots.last))
                }
              )

            runMotion(
              numberToGo - 1,
              newWipKnots,
              wipVisited + newWipKnots.last
            )
          }
        }

        val (newKnots, newVisited) =
          runMotion(motion.number, knots, visited)

        goThroughTheMotions(
          index + 1,
          newKnots,
          newVisited
        )
      }
    }

    val startKnots = Vector.fill(numberOfKnots)(Point.startPosition)
    val pointsVisited = Set(Point.startPosition)

    goThroughTheMotions(0, startKnots, pointsVisited)
  }

  def main(args: Array[String]): Unit = {
    println(countVisitedPositions(filenamePart1Sample, 2))
    println(countVisitedPositions(filenamePart1Input, 2))

    println(countVisitedPositions(filenamePart1Sample, 10))
    println(countVisitedPositions(filenamePart1Input, 10))
  }
}
