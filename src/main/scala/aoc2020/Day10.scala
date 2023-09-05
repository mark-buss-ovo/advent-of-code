package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  private val filenamePart1Sample1 = "2020/Day10/part1-sample1.txt"
  private val filenamePart1Sample2 = "2020/Day10/part1-sample2.txt"
  private val filenamePart1Input = "2020/Day10/part1-input.txt"

  private def parseAdapters(filename: String): Map[Int, List[Int]] = {
    val sortedRatings = Source
      .fromResource(filename)
      .getLines()
      .map(l => l.toInt)
      .toList
      .sorted
      .reverse

    val deviceAdapterRating = sortedRatings.head + 3

    val allRatings = deviceAdapterRating +: sortedRatings :+ 0

    allRatings
      .foldLeft(Map.empty[Int, List[Int]])((adapters, rating) => {
        adapters.updated(
          rating,
          adapters.keys.filter(r => (r - 3 until r).contains(rating)).toList
        )
      })
  }

  private def getDifferences(filename: String): Int = {
    val adapters = parseAdapters(filename)

    @tailrec
    def chainAdapters(
        remaining: Map[Int, List[Int]],
        differences: Map[Int, Int],
        currentRating: Int
    ): Int = {

      val nextRating = remaining(currentRating).min

      val difference = nextRating - currentRating

      val existingCount = differences.getOrElse(difference, 0)
      val updatedDifferences =
        differences.updated(difference, existingCount + 1)

      val updatedRemaining = remaining.removed(currentRating)

      if (updatedRemaining.size > 1) {
        chainAdapters(
          updatedRemaining,
          updatedDifferences,
          nextRating
        )
      } else {
        val count1 = updatedDifferences(1)
        val count3 = updatedDifferences(3)

        count1 * count3
      }
    }

    chainAdapters(adapters, Map.empty, 0)
  }

  private case class BuildPathOutcome(
      paths: Set[Vector[Int]],
      checkedRatings: Set[Int]
  ) {
    def addRating(rating: Int): BuildPathOutcome = {
      copy(paths, checkedRatings + rating)
    }

    def merge(other: BuildPathOutcome): BuildPathOutcome = {
      BuildPathOutcome(
        paths.union(other.paths),
        checkedRatings.union(other.checkedRatings)
      )
    }
  }

  private def findPossibleCombinations(filename: String): Long = {
    val adapters = parseAdapters(filename)

    val adaptersWithBranches = adapters.filter { case (_, connectsTo) =>
      connectsTo.size > 1
    }

    println(adaptersWithBranches)

    val orderedBranchingRatings = adaptersWithBranches.keys.toList.sorted

    def buildPaths(
        rating: Int,
        basePath: Vector[Int],
        pathsSoFar: BuildPathOutcome
    ): BuildPathOutcome = {
      val newBasePath = basePath :+ rating

      val branches = adaptersWithBranches.get(rating)

      branches match {
        case Some(branchRatings) =>
          branchRatings.foldLeft(
            pathsSoFar.addRating(rating)
          )((paths, branchRating) => {
            val outcome =
              buildPaths(branchRating, newBasePath, paths)
            paths.merge(outcome)
          })
        case None =>
          BuildPathOutcome(
            pathsSoFar.paths + newBasePath,
            pathsSoFar.checkedRatings + rating
          )
      }
    }

    val foldResult = orderedBranchingRatings.foldLeft((1L, Set.empty[Int])) {
      case ((count, checkedRatings), rating) =>
        if (checkedRatings.contains(rating)) {
          (count, checkedRatings)
        } else {
          val outcome = buildPaths(
            rating,
            Vector.empty,
            BuildPathOutcome(Set.empty, Set.empty)
          )
          (
            count * outcome.paths.size,
            checkedRatings.union(outcome.checkedRatings)
          )
        }
    }

    foldResult match {
      case (count, _) => count
    }
  }

  def main(args: Array[String]): Unit = {
    println(getDifferences(filenamePart1Sample1))
    println(getDifferences(filenamePart1Sample2))
    println(getDifferences(filenamePart1Input))

    println(findPossibleCombinations(filenamePart1Sample1))
//    println(findPossibleCombinations(filenamePart1Sample2))
//    println(findPossibleCombinations(filenamePart1Input))
  }
}
