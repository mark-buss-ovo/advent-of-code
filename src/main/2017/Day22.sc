import scala.io.Source

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day22-input.txt")

val startingGrid = bufferedSource.getLines.foldLeft(Vector.empty[Vector[Boolean]])((rows, line) => {
  if (line.isEmpty) {
    rows
  }
  else {
    println
    rows.appended(line.map(c => c == '#').toVector)
  }
})

def printGrid(grid: Vector[Vector[Boolean]]): Unit = {
  println(grid.map(row => row.map(c => if(c)'#'else'.').mkString).mkString("\n"))
}

object Direction extends Enumeration {
  type Direction = Value

  val Down, Up, Left, Right = Value
}

import Direction._

case class Point(x: Int, y: Int)

val centre = startingGrid.length/2
val startingPoint = Point(centre, centre)

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
    case Down => Point(point.x, point.y + 1)
    case Up => Point(point.x, point.y - 1)
    case Left => Point(point.x - 1, point.y)
    case Right => Point(point.x + 1, point.y)
  }
}

def burst(grid: Vector[Vector[Boolean]], currentPoint: Point, direction: Direction, infectionCount: Int, burstsSoFar: Int, totalBursts: Int): Int = {
  val alreadyInfected = grid(currentPoint.y)(currentPoint.x)
  val nextDirection = if(alreadyInfected) turnRight(direction) else turnLeft(direction)
  val newGrid = grid.indices.map(y => grid(y).indices.map(x => {
    if(x == currentPoint.x && y == currentPoint.y) {
      !alreadyInfected
    } else {
      grid(y)(x)
    }
  }))

  val nextPoint = getNextPoint(currentPoint, nextDirection)

  val (expandedGrid: Vector[Vector[Boolean]], pointAfterExpand) = nextPoint match {
    case Point(x, _) if x < 0 =>
      (newGrid.map(row => false +: row), Point(nextPoint.x + 1, nextPoint.y))
    case Point(x, _) if x == newGrid.head.length =>
      (newGrid.map(row => row :+ false), nextPoint)
    case Point(_, y) if y < 0 =>
      (newGrid.head.map(_ => false) +: newGrid, Point(nextPoint.x, nextPoint.y + 1))
    case Point(_, y) if y == newGrid.length =>
      (newGrid :+ newGrid.head.map(_ => false), nextPoint)
    case _ => (newGrid, nextPoint)
  }

  val incremented = burstsSoFar + 1
  val newInfectedCount = if(alreadyInfected) infectionCount else infectionCount + 1
  if(incremented == totalBursts) {
    newInfectedCount
  } else {
    burst(expandedGrid, pointAfterExpand, nextDirection, newInfectedCount, incremented, totalBursts)
  }
}

burst(startingGrid, startingPoint, Up, 0, 0, 10000)