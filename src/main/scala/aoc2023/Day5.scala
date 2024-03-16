package aoc2023

import scala.annotation.tailrec
import scala.io.Source

object Day5 {

  private val filenamePart1Sample = "2023/Day5/part1-sample.txt"
  private val filenamePart1Input = "2023/Day5/part1-input.txt"

  private case class Range(
      destinationRangeStart: Long,
      sourceRangeStart: Long,
      rangeLength: Long
  ) {
    def getDestination(sourceValue: Long): Option[Long] = {
      if (
        sourceValue >= sourceRangeStart && sourceValue <= sourceRangeStart + rangeLength
      ) {
        val offset = sourceValue - sourceRangeStart
        Some(destinationRangeStart + offset)
      } else None
    }
  }

  case class Window(
      sourceStart: Long,
      sourceEnd: Long,
      destStart: Long,
      destEnd: Long
  ) {
    def getNextWindow(start: Long, end: Long): Option[(Long, Long)] = {
      if (start > sourceEnd || end < sourceStart) {
        None
      } else if (start < sourceStart && end > sourceEnd) {
        // source window completely contains this window, so return it
        Some((this.destStart, this.destEnd))
      } else if (start >= sourceStart && end <= sourceEnd) {
        // this window completely contains source window so just return the sub-window it covers
        val offset = start - sourceStart
        val length = end - start
        val newDestStart = destStart + offset
        val newDestEnd = newDestStart + length
        Some((newDestStart, newDestEnd))
      } else if (start <= sourceStart && end > sourceStart) {
        // window overlaps the start so need to return the part that overlaps
        val diff = end - sourceStart
        Option((destStart, destStart + diff))
      } else {
        // window overlaps the end so need to return the part that overlaps
        val leftDiff = start - sourceStart
        val newDestStart = destStart + leftDiff

        Option((newDestStart, destEnd))
      }
    }
  }

  private case class Map(
      sourceType: String,
      destinationType: String,
      ranges: List[Range]
  ) {
    def getDestination(source: Long): Long =
      ranges
        .map(_.getDestination(source))
        .collectFirst { case Some(dest) =>
          dest
        }
        .getOrElse(source)

    private val windows: List[Window] = {
      val orderedRanges = ranges.sortBy(_.sourceRangeStart)

      @tailrec
      def buildWindows(
          windowsSoFar: List[Window],
          remainingRanges: List[Range],
          nextWindowStart: Long
      ): List[Window] = {
        remainingRanges.headOption match {
          case Some(nextRange) =>
            val nextRangeWindow = Window(
              nextRange.sourceRangeStart,
              nextRange.sourceRangeStart + nextRange.rangeLength - 1,
              nextRange.destinationRangeStart,
              nextRange.destinationRangeStart + nextRange.rangeLength - 1
            )

            val windowsToAppend =
              if (nextRange.sourceRangeStart == nextWindowStart) {
                // The next range starts where the previous one left off so use it for the next window
                List(nextRangeWindow)
              } else {
                // There is a gap between the last range and the next one so add a window in between
                List(
                  Window(
                    nextWindowStart,
                    nextRange.sourceRangeStart - 1,
                    nextWindowStart,
                    nextRange.sourceRangeStart - 1
                  ),
                  nextRangeWindow
                )
              }

            buildWindows(
              windowsSoFar.appendedAll(windowsToAppend),
              remainingRanges.tail,
              nextRange.sourceRangeStart + nextRange.rangeLength
            )
          case None =>
            // reached the end of our ranges so add a final window from the last point to infinity (effectively) and return
            windowsSoFar.appended(
              Window(
                nextWindowStart,
                Long.MaxValue,
                nextWindowStart,
                Long.MaxValue
              )
            )
        }
      }

      buildWindows(List.empty, orderedRanges, 0)
    }

    def getNextWindows(start: Long, end: Long): List[(Long, Long)] = {
      windows.flatMap(_.getNextWindow(start, end))
    }
  }

  private case class Almanac(seedNumbers: List[Long], maps: List[Map]) {
    def getLowestLocationNumber: Long = {

      @tailrec
      def traverseMapsForward(sourceValue: Long, sourceType: String): Long = {
        val nextMap = maps.find(_.sourceType == sourceType)

        nextMap match {
          case Some(map) =>
            val nextValue = map.getDestination(sourceValue)
            traverseMapsForward(nextValue, map.destinationType)
          case None => sourceValue
        }
      }

      seedNumbers.map(traverseMapsForward(_, "seed")).min
    }
  }

  private def parseAlmanac(filename: String): Almanac = {
    val lines = Source.fromResource(filename).getLines().toList

    val seedNumbers =
      lines.head.substring("seeds: ".length).split(" ").map(_.toLong).toList

    Almanac(seedNumbers, parseMaps(lines))
  }

  private def parseMaps(lines: List[String]) = {
    val mapChunks =
      lines.drop(2).mkString("\n").split("\n\n").map(_.split("\n"))

    mapChunks
      .map(mapLines => {
        mapLines.head match {
          case s"$sourceType-to-$destinationType map:" =>
            val ranges = mapLines.tail.map {
              case s"$destinationRangeStart $sourceRangeStart $rangeLength" =>
                Range(
                  destinationRangeStart.toLong,
                  sourceRangeStart.toLong,
                  rangeLength.toLong
                )
            }.toList
            Map(sourceType, destinationType, ranges)
        }
      })
      .toList
  }

  private case class AlmanacPart2(
      seedRanges: List[(Long, Long)],
      maps: List[Map]
  ) {
    def getLowestLocationNumber: Long = {

      @tailrec
      def traverseMaps(
          sourceType: String,
          windowsToTraverse: List[(Long, Long)]
      ): Long = {
        maps.find(_.sourceType == sourceType) match {
          case Some(map) =>
            val newWindows =
              windowsToTraverse.foldLeft(List.empty[(Long, Long)])(
                (newWindows, window) => {
                  window match {
                    case (start, end) =>
                      newWindows.appendedAll(map.getNextWindows(start, end))
                  }
                }
              )
            traverseMaps(map.destinationType, newWindows)
          case None =>
            // We're at the end of the maps so just find the lowest location for all windows
            windowsToTraverse.map { case (start, _) => start }.min
        }
      }

      traverseMaps("seed", seedRanges)
    }
  }

  private def parseAlmanacPart2(filename: String): AlmanacPart2 = {
    val lines = Source.fromResource(filename).getLines().toList

    val seedRanges = lines.head match {
      case s"seeds: $seedRanges" =>
        seedRanges
          .split(" ")
          .grouped(2)
          .map(g => {
            val start = g(0).toLong
            val length = g(1).toLong
            (start, start + length - 1)
          })
          .toList
    }

    AlmanacPart2(seedRanges, parseMaps(lines))
  }

  def main(args: Array[String]): Unit = {
    println(
      s"Part 1 Sample: ${parseAlmanac(filenamePart1Sample).getLowestLocationNumber}"
    )
    println(
      s"Part 1 Input: ${parseAlmanac(filenamePart1Input).getLowestLocationNumber}"
    )
    println(
      s"Part 2 Sample: ${parseAlmanacPart2(filenamePart1Sample).getLowestLocationNumber}"
    )
    println(
      s"Part 2 Input: ${parseAlmanacPart2(filenamePart1Input).getLowestLocationNumber}"
    )
  }

}
