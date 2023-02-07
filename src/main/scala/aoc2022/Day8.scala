package aoc2022

import scala.annotation.tailrec
import scala.io.Source

object Day8 {
  private val filenamePart1Sample = "2022/Day8/part1-sample.txt"
  private val filenamePart1Input = "2022/Day8/part1-input.txt"

  case class TreeGrid(trees: Vector[Vector[Int]]) {
    private def allCoords: Vector[(Int, Int)] = {
      (for {
        y <- trees.indices
        x <- trees.head.indices
      } yield (x, y)).toVector
    }

    private def onTheEdge(x: Int, y: Int) = {
      x == 0 || y == 0 || x == trees.head.length - 1 || y == trees.length - 1
    }

    private def getAboveBelowLeftRightTrees(
        x: Int,
        y: Int
    ): Vector[Vector[Int]] = {
      val treesAbove =
        (0 until y).map(otherY => trees(otherY)(x)).reverse.toVector
      val treesBelow =
        (y + 1 until trees.length).map(otherY => trees(otherY)(x)).toVector
      val treesLeft =
        (0 until x).map(otherX => trees(y)(otherX)).reverse.toVector
      val treesRight =
        (x + 1 until trees.head.length).map(otherX => trees(y)(otherX)).toVector

      Vector(treesAbove, treesBelow, treesLeft, treesRight)
    }

    def numberVisible(): Int = {
      allCoords.count { case (x, y) =>
        if (onTheEdge(x, y)) true
        else {
          val tree = trees(y)(x)

          val directionTrees =
            getAboveBelowLeftRightTrees(x, y)

          directionTrees.exists(otherTrees => otherTrees.forall(_ < tree))
        }
      }
    }

    def highestScenicScore(): Int = {
      val allScores = allCoords.map { case (x, y) =>
        if (onTheEdge(x, y)) 0
        else {
          val tree = trees(y)(x)

          val directionTrees = getAboveBelowLeftRightTrees(x, y)

          @tailrec
          def traverseTrees(otherTrees: Vector[Int], index: Int = 0): Int = {
            if (index > otherTrees.length - 1)
              index
            else {
              val otherTree = otherTrees(index)
              if (otherTree < tree) { // tree is smaller so keep going
                traverseTrees(otherTrees, index + 1)
              } else {
                index + 1
              }
            }
          }

          val scores = directionTrees.map((otherTrees: Vector[Int]) =>
            traverseTrees(otherTrees)
          )
          scores.tail.foldLeft(scores.head)((acc, score) => acc * score)
        }
      }
      allScores.max
    }

  }

  def parseInput(filename: String): TreeGrid = {
    val lines = Source.fromResource(filename).getLines().toVector
    TreeGrid(lines.map(l => l.map(c => c.asDigit).toVector))
  }

  def main(args: Array[String]): Unit = {
    val sampleGrid = parseInput(filenamePart1Sample)
    println(sampleGrid.numberVisible())
    println(sampleGrid.highestScenicScore())

    val inputGrid = parseInput(filenamePart1Input)
    println(inputGrid.numberVisible())
    println(inputGrid.highestScenicScore())
  }
}
