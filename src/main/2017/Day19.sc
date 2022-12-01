import scala.io.Source

val bufferedSource = Source.fromFile("/Users/mark.buss/Dev/algorithmic-code-club/advent-of-code/src/main/2017/Day19-input.txt")

val pathGrid = bufferedSource.getLines.foldLeft(Vector.empty[String])((grid, line) => {
  if (line.isEmpty) {
    grid
  }
  else {
    grid.appended(line)
  }
})

object Direction extends Enumeration {
  type Direction = Value

  val Down, Up, Left, Right = Value
}

import Direction._

case class Point(x: Int, y: Int)

def getNextPoint(point: Point, direction: Direction): Point = {
  direction match {
    case Down => Point(point.x, point.y + 1)
    case Up => Point(point.x, point.y - 1)
    case Left => Point(point.x - 1, point.y)
    case Right => Point(point.x + 1, point.y)
  }
}

def getNewDirection(point: Point, currentDirection: Direction.Direction): Direction = {
  currentDirection match {
    case Down | Up => if(pathGrid(point.y)(point.x - 1)!= ' ') Left else Right
    case Left | Right => if(pathGrid(point.y - 1)(point.x) != ' ') Up else Down
  }
}

def traversePath(point: Point, direction: Direction, lettersSoFar: String, stepCount: Int): (String, Int) = {
  val newStepCount = stepCount + 1
  val nextPoint = getNextPoint(point, direction)
  val nextChar = pathGrid(nextPoint.y)(nextPoint.x)
  nextChar match {
    case l if l.isUpper => traversePath(nextPoint, direction, lettersSoFar + l, newStepCount)
    case '|' | '-' => traversePath(nextPoint, direction, lettersSoFar, newStepCount)
    case '+' =>
      val newDirection = getNewDirection(nextPoint, direction)
      traversePath(nextPoint, newDirection, lettersSoFar, newStepCount)
    case ' ' =>
      (lettersSoFar, newStepCount)
  }
}

val startingPoint = Point(pathGrid(0).indexOf('|'), 0)
traversePath(startingPoint, Down, "", 0)