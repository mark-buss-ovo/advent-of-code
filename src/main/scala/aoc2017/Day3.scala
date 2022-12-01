package aoc

object Day3 {

  case class Point(x: Int, y: Int)

  object Direction extends Enumeration {
    type Direction = Value

    val Down, Up, Left, Right = Value
  }

  import Direction._

  def getNextIteration(
      direction: Direction,
      point: Point,
      grid: Map[Point, Int]
  ): (Direction, Point) = {
    direction match {
      case Right =>
        if (grid.contains(point.copy(y = point.y - 1)))
          (Right, point.copy(x = point.x + 1))
        else (Up, point.copy(y = point.y - 1))
      case Up =>
        if (grid.contains(point.copy(x = point.x - 1)))
          (Up, point.copy(y = point.y - 1))
        else (Left, point.copy(x = point.x - 1))
      case Left =>
        if (grid.contains(point.copy(y = point.y + 1)))
          (Left, point.copy(x = point.x - 1))
        else (Down, point.copy(y = point.y + 1))
      case Down =>
        if (grid.contains(point.copy(x = point.x + 1)))
          (Down, point.copy(y = point.y + 1))
        else (Right, point.copy(x = point.x + 1))
    }
  }

  def getDistance(
      targetValue: Int,
      grid: Map[Point, Int],
      value: Int,
      coordinate: Point,
      direction: Direction
  ): Int = {
    val updatedGrid = grid.updated(coordinate, value)

    if (value == targetValue) {
      Math.abs(coordinate.x) + Math.abs(coordinate.y)
    } else {
      getNextIteration(direction, coordinate, updatedGrid) match {
        case (nextDirection, nextPoint) =>
          getDistance(
            targetValue,
            updatedGrid,
            value + 1,
            nextPoint,
            nextDirection
          )
      }
    }
  }

  def getFirstGreaterThan(
      targetValue: Int,
      grid: Map[Point, Int],
      coordinate: Point,
      direction: Direction
  ): Int = {
    val allAdjacentPoints = Vector(
      coordinate.copy(x = coordinate.x - 1),
      coordinate.copy(x = coordinate.x - 1, y = coordinate.y + 1),
      coordinate.copy(y = coordinate.y + 1),
      coordinate.copy(x = coordinate.x + 1, y = coordinate.y + 1),
      coordinate.copy(x = coordinate.x + 1),
      coordinate.copy(x = coordinate.x + 1, y = coordinate.y - 1),
      coordinate.copy(y = coordinate.y - 1),
      coordinate.copy(x = coordinate.x - 1, y = coordinate.y - 1)
    )

    val sum = allAdjacentPoints.foldLeft(0)((total, point) =>
      total + grid.getOrElse(point, 0)
    )

    if (sum > targetValue) sum
    else {
      val updatedGrid = grid.updated(coordinate, sum)
      getNextIteration(direction, coordinate, updatedGrid) match {
        case (nextPoint, nextDirection) =>
          getFirstGreaterThan(
            targetValue,
            updatedGrid,
            nextDirection,
            nextPoint
          )
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val startGrid = Map.empty[Point, Int]
//    println(getDistance(1, startGrid, 1, Point(0, 0), Right))
//    println(getDistance(12, startGrid, 1, Point(0, 0), Right))
//    println(getDistance(23, startGrid, 1, Point(0, 0), Right))
//    println(getDistance(1024, startGrid, 1, Point(0, 0), Right))
//    println(getDistance(277678, startGrid, 1, Point(0, 0), Right))

    val startGridPart2 = Map(Point(0, 0) -> 1)
    println(getFirstGreaterThan(277678, startGridPart2, Point(1, 0), Right))
  }
}
