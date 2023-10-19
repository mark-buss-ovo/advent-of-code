package aoc2020

import aoc2020.Day20.CornerType._

import scala.annotation.tailrec
import scala.io.Source

object Day20 {
  private val filenamePart1Sample = "2020/Day20/part1-sample.txt"
  private val filenamePart1Input = "2020/Day20/part1-input.txt"

  sealed trait CornerType
  object CornerType {
    case object TopLeft extends CornerType
    case object TopRight extends CornerType
    case object BottomLeft extends CornerType
    case object BottomRight extends CornerType
  }

  private case class Image(rows: List[String]) {

    def rotate: Image = {
      val newRows: List[String] = {
        rows.indices.map(index => {
          rows.indices.reverse
            .map(reverseIndex => {
              rows(reverseIndex)(index)
            })
            .mkString
        })
      }.toList
      Image(newRows)
    }

    def flipHorizontal: Image = {
      val newRows: List[String] = {
        rows.map(_.reverse)
      }
      Image(newRows)
    }

    def flipVertical: Image = {
      val newRows: List[String] = rows.reverse
      Image(newRows)
    }

    val top: String = rows.head
    val bottom: String = rows.last
    val left: String = rows.map(_.head).mkString
    val right: String = rows.map(_.last).mkString

    def removeBorders(): Image = {
      Image(
        rows
          .slice(1, rows.size - 1)
          .map(r => r.substring(1, r.length - 1))
      )
    }

    def slidingWindows(width: Int, height: Int): List[String] = {
      (0 to rows.length - height).foldLeft(List.empty[String])(
        (allWindows, y) => {
          val rowSubSet = rows.slice(y, y + height)

          val rowWindows =
            (0 to rowSubSet.head.length - width).foldLeft(List.empty[String])(
              (windows, x) => {
                val substrings = rowSubSet.map(s => s.substring(x, x + width))

                windows.appended(
                  substrings.mkString("\n")
                )
              }
            )

          allWindows.appendedAll(rowWindows)
        }
      )
    }
  }

  private case class Tile(id: Long, image: Image) {
    val permutations: List[Image] = {
      val rotations = List(
        image,
        image.rotate,
        image.rotate.rotate,
        image.rotate.rotate.rotate
      )
      rotations.foldLeft(List.empty[Image])((allImages, thisImage) =>
        allImages.appendedAll(
          List(thisImage, thisImage.flipVertical, thisImage.flipHorizontal)
        )
      )
    }
  }
  private val tileIdRegex = """^Tile (\d+):""".r

  private def parseTiles(filename: String): List[Tile] = {
    val contents = Source.fromResource(filename).mkString

    contents
      .split("\n\n")
      .map(_.split('\n'))
      .map(tileStrings => {
        val id = tileStrings.head match {
          case tileIdRegex(idString) => idString.toLong
        }

        val image = Image(tileStrings.tail.toList)

        Tile(id, image)
      })
      .toList
  }

  private def findCorners(tiles: List[Tile]): List[(Tile, CornerType)] = {
    tiles.flatMap(t => {

      val otherTiles = tiles.filterNot(other => other == t)

      val topMatches = otherTiles.count(other =>
        other.permutations.exists(p => p.bottom == t.image.top)
      )

      val bottomMatches = otherTiles.count(other =>
        other.permutations.exists(p => p.top == t.image.bottom)
      )

      val leftMatches = otherTiles.count(other =>
        other.permutations.exists(p => p.right == t.image.left)
      )

      val rightMatches = otherTiles.count(other =>
        other.permutations.exists(p => p.left == t.image.right)
      )

      (topMatches, bottomMatches, leftMatches, rightMatches) match {
        case (0, 1, 0, 1) => Some((t, TopLeft))
        case (0, 1, 1, 0) => Some((t, TopRight))
        case (1, 0, 0, 1) => Some((t, BottomLeft))
        case (1, 0, 1, 0) => Some((t, BottomRight))
        case _            => None
      }
    })
  }

  private def multiplyCorners(filename: String): Long = {
    val tiles = parseTiles(filename)
    val corners = findCorners(tiles)
    corners.map { case (tile, _) =>
      tile.id
    }.product
  }

  case class Point(x: Int, y: Int) {
    def getNext(gridSize: Int): Point = {
      if (x < gridSize - 1) copy(x = x + 1)
      else Point(x = 0, y = y + 1)
    }
  }

  private def pieceTogether(filename: String): Map[Point, Image] = {
    val tiles = parseTiles(filename)

    val gridSize = Math.sqrt(tiles.size).toInt

    val corners = findCorners(tiles)
    val startingTile = corners.collectFirst {
      case (tile, cornerType) if cornerType == TopLeft => tile
    }.get

    val startingGrid = Map(Point(0, 0) -> startingTile.image)

    @tailrec
    def fillInGrid(
        grid: Map[Point, Image],
        point: Point,
        remainingTiles: List[Tile]
    ): Map[Point, Image] = {

      val tileMatches = if (point.x == 0) {
        // at the start of the line so need to look at tile above
        val above = grid(Point(0, point.y - 1))
        remainingTiles.map(t =>
          (t, t.permutations.filter(p => p.top == above.bottom).toSet)
        )
      } else {
        val left = grid(Point(point.x - 1, point.y))
        remainingTiles.map(t =>
          (t, t.permutations.filter(p => p.left == left.right).toSet)
        )
      }

      val possibleMatches = tileMatches.filterNot { case (_, imageMatches) =>
        imageMatches.isEmpty
      }

      if (possibleMatches.isEmpty) {
        throw new Exception("No matches")
      }

      if (
        possibleMatches.size > 1 || possibleMatches.exists {
          case (_, imageMatches) => imageMatches.size > 1
        }
      )
        throw new Exception("Too many matches")

      possibleMatches.head match {
        case (otherTile, matchingImages) =>
          val updatedGrid = grid.updated(point, matchingImages.head)
          val updatedTiles = remainingTiles.filterNot(t => t == otherTile)
          val nextPoint = point.getNext(gridSize)
          if (nextPoint.y > gridSize - 1) {
            // reached the end of the grid so exit out
            updatedGrid
          } else {
            fillInGrid(updatedGrid, nextPoint, updatedTiles)
          }
      }

    }

    fillInGrid(
      startingGrid,
      Point(1, 0),
      tiles.filterNot(t => t == startingTile)
    )
  }

  private def composeImage(grid: Map[Point, Image]): Image = {
    val xs = grid.keys.map(_.x)
    val ys = grid.keys.map(_.y)

    val composedRows =
      (ys.min to ys.max).foldLeft(List.empty[String])((rows, y) => {
        val newRows =
          (xs.min to xs.max).foldLeft(
            List.fill(grid(Point(xs.min, ys.min)).rows.size)("")
          )((subRows, x) => {
            val original = grid(Point(x, y))
            val subImage = original.removeBorders()

            val combined = subRows.zip(subImage.rows).map { case (s1, s2) =>
              s"$s1$s2"
            }

            combined
          })
        rows.appendedAll(newRows)
      })

    Image(composedRows)
  }

  private def seaMonsterRegex =
    "..................#.\\n#....##....##....###\\n.#..#..#..#..#..#...".r

  private def findSeaMonsters(filename: String): Int = {
    val grid = pieceTogether(filename)
    val composed = composeImage(grid)
    val uberTile = Tile(-1L, composed)

    @tailrec
    def countRoughSea(remainingPermutations: List[Image]): Int = {
      val currentPermutation = remainingPermutations.head

      val searchWindows = currentPermutation.slidingWindows(20, 3)

      val seaMonsterCount = searchWindows.count(s => seaMonsterRegex.matches(s))
      if (seaMonsterCount > 0) {
        // 15 # in sea monster, multiply count by 15, take away from total # in image
        val allHash = currentPermutation.rows.map(_.count(c => c == '#')).sum
        val seaMonsterHash = 15 * seaMonsterCount
        val waterCount = allHash - seaMonsterHash
        waterCount
      } else {
        countRoughSea(remainingPermutations.tail)
      }
    }

    countRoughSea(uberTile.permutations)
  }

  def main(args: Array[String]): Unit = {
    println(s"Part 1 Sample: ${multiplyCorners(filenamePart1Sample)}")
    println(s"Part 1 Input: ${multiplyCorners(filenamePart1Input)}")
    println(s"Part 2 Sample: ${findSeaMonsters(filenamePart1Sample)}")
    println(s"Part 2 Input: ${findSeaMonsters(filenamePart1Input)}")
  }
}
