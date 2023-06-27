package aoc2022

import aoc2022.Day11.Monkey
import aoc2022.Day11.Operator.{Add, Multiply, fromString}

import scala.None
import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.Source

object Day12 {

  private val filenamePart1Sample = "2022/Day12/part1-sample.txt"
  private val filenamePart1Input = "2022/Day12/part1-input.txt"

  private val heightChart = "abcdefghijklmnopqrstuvwxyz"

  case class GridSquare(elevation: Int, isStart: Boolean, isTarget: Boolean)

  case class Point(x: Int, y: Int) {
    def getAdjacent(maxX: Int, maxY: Int): List[Point] = {
      List(copy(x = x - 1), copy(x = x + 1), copy(y = y - 1), copy(y = y + 1))
        .filter(p => p.x >= 0 && p.x <= maxX)
        .filter(p => p.y >= 0 && p.y <= maxY)
    }
  }
  def parseInput(filename: String): Map[Point, GridSquare] = {
    val lines = Source.fromResource(filename).getLines()

    lines.foldLeft(Map.empty[Point, GridSquare])((grid, line) => {
      val y = if (grid.isEmpty) 0 else grid.keys.map(_.y).max + 1

      grid ++ line.foldLeft(Map.empty[Point, GridSquare])((lineGrid, char) => {
        val x = if (lineGrid.isEmpty) 0 else lineGrid.keys.map(_.x).max + 1
        val gridSquare = char match {
          case 'S' => GridSquare(0, true, false)
          case 'E' => GridSquare(25, false, true)
          case _   => GridSquare(heightChart.indexOf(char), false, false)
        }

        lineGrid.updated(Point(x, y), gridSquare)
      })
    })
  }

  case class Node(location: Point, pathLength: Int, parent: Option[Node]) {
    def getAdjacent(maxX: Int, maxY: Int): Vector[Node] = {
      location
        .getAdjacent(maxX, maxY)
        .map(l => Node(l, pathLength + 1, Some(this)))
        .toVector
    }
  }

  def getPathSize(node: Node, pathSize: Int = 0): Int = {
    node.parent match {
      case Some(parent) => getPathSize(parent, pathSize + 1)
      case None         => pathSize
    }
  }

  def findShortestRouteFromS(filename: String): Int = {
    findShortestRouteFrom(
      filename,
      gridSquare => gridSquare.isStart
    )
  }

  def findShortestRouteFromLowestPoints(filename: String) = {
    findShortestRouteFrom(
      filename,
      gridSquare => gridSquare.isStart || gridSquare.elevation == 0
    )
  }

  private def findShortestRouteFrom(
      filename: String,
      isStartingPoint: (GridSquare) => Boolean
  ) = {
    val grid = parseInput(filename)

    val startingPoints = grid.collect {
      case (p, s) if isStartingPoint(s) => p
    }

    val maxX = grid.keys.map(_.x).max
    val maxY = grid.keys.map(_.y).max

    @tailrec
    def processNode(
        currentNode: Node,
        processedSoFar: Vector[Node],
        alreadyVisitedOnThisRoute: Set[Point]
    ): Option[Int] = {
      val currentSquare = grid(currentNode.location)

      if (currentSquare.isTarget)
        // reached the target so stop here
        Some(getPathSize(currentNode))
      else {

        val nodesCouldVisitNext = currentNode
          .getAdjacent(maxX, maxY)
          .filterNot(n => alreadyVisitedOnThisRoute.contains(n.location))
          .filter(n => {
            val square = grid(n.location)
            square.elevation <= currentSquare.elevation + 1
          })

        val newNodes = nodesCouldVisitNext.filterNot(n =>
          processedSoFar.exists(_.location == n.location)
        )
        val updatedProcessedSoFar = (processedSoFar.map(processed => {
          val maybeNewerVersion =
            nodesCouldVisitNext.find(n => n.location == processed.location)
          maybeNewerVersion match {
            case Some(newerVersion) =>
              if (newerVersion.pathLength < processed.pathLength) newerVersion
              else processed
            case None => processed
          }
        }) ++ newNodes)
          .filterNot(p => p.location == currentNode.location)
          .sortBy(_.pathLength)

        if (updatedProcessedSoFar.isEmpty)
          None
        else
          processNode(
            updatedProcessedSoFar.head,
            updatedProcessedSoFar,
            alreadyVisitedOnThisRoute + currentNode.location
          )
      }
    }

    startingPoints
      .flatMap(p => processNode(Node(p, 0, None), Vector.empty, Set.empty))
      .min
  }

  def main(args: Array[String]): Unit = {
    println(findShortestRouteFromS(filenamePart1Sample))
    println(findShortestRouteFromS(filenamePart1Input))
    println(findShortestRouteFromLowestPoints(filenamePart1Sample))
    println(findShortestRouteFromLowestPoints(filenamePart1Input))
  }
}
