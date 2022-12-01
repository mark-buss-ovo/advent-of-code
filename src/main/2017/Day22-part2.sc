import scala.annotation.tailrec
import scala.io.Source

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day22-sample.txt")

sealed trait Node

object Node {
  case object Clean extends Node
  case object Infected extends Node
  case object Weakened extends Node
  case object Flagged extends Node
}

import Node._
case class Point(x: Int, y: Int)


val startingGrid = bufferedSource.getLines.foldLeft(Map.empty[Point, Node])((grid, line) => {
  if (line.isEmpty) {
    grid
  }
  else {
    val x = 0
    val y = if(grid.isEmpty) 0 else grid.keys.map(p => p.y).min - 1
    line.indices.foldLeft(grid)((newGrid, index) => {
      val node = if(line(index) == '#') Infected else Clean
      val point = Point(x + index, y)
      newGrid.updated(point, node)
    })
  }
})

def printGrid(grid: Map[Point, Node]) : Unit = {
  val points = grid.keys
  val ys = points.map(_.y).toVector.sorted.reverse

  val rows = for {
    y <- ys
  } yield grid.collect {
    case (point, node) if point.y == y => (point.x, node)
  }

  val rowStrings = rows.foldLeft(Vector.empty[String])((grid, rowMap) => {
    val keys = rowMap.keys.toVector.sorted
    val string = keys.map(x => {
      rowMap(x) match {
        case Infected => '#'
        case Weakened => 'W'
        case Clean => '.'
        case Flagged => 'F'
      }
    }).mkString
    grid.appended(string)
  })

  println(rowStrings.mkString("\n"))
}

sealed trait Direction
object Direction {
  case object Down extends Direction
  case object Up extends Direction
  case object Left extends Direction
  case object Right extends Direction
}

import Direction._


val centreX = startingGrid.keys.map(k => k.x).max / 2
val centreY = startingGrid.keys.map(k => k.y).min / 2
val startingPoint = Point(centreX, centreY)

def turnLeft(direction: Direction): Direction = {
  direction match {
    case Down => Right
    case Up => Left
    case Left => Down
    case Right => Up
  }
}

def turnRight(direction: Direction): Direction = {
  direction match {
    case Down => Left
    case Up => Right
    case Left => Up
    case Right => Down
  }
}

def getNextPoint(point: Point, direction: Direction): Point = {
  direction match {
    case Down => Point(point.x, point.y - 1)
    case Up => Point(point.x, point.y + 1)
    case Left => Point(point.x - 1, point.y)
    case Right => Point(point.x + 1, point.y)
  }
}

def reverse(direction: Direction): Direction = {
  direction match {
    case Up => Down
    case Down => Up
    case Left => Right
    case Right => Left
  }
}

@tailrec
def burst(grid: Map[Point, Node], currentPoint: Point, direction: Direction, infectionCount: Int, burstsSoFar: Int, totalBursts: Int): Int = {
  val currentState = grid.getOrElse(currentPoint, Clean)
  val (nextDirection, nextState) = currentState match {
    case Clean => (turnLeft(direction), Weakened)
    case Weakened => (direction, Infected)
    case Infected => (turnRight(direction), Flagged)
    case Flagged => (reverse(direction), Clean)
  }

  val incremented = burstsSoFar + 1
  val newInfectedCount = if(nextState == Infected) infectionCount + 1 else infectionCount
  if(incremented == totalBursts) {
    newInfectedCount
  } else {
    val newGrid = grid.updated(currentPoint, nextState)
    val nextPoint = getNextPoint(currentPoint, nextDirection)

    burst(newGrid, nextPoint, nextDirection, newInfectedCount, incremented, totalBursts)
  }
}

burst(startingGrid, startingPoint, Up, 0, 0, 10000000)