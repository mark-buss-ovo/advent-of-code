package aoc
import scala.io.Source

object Day2 {

  def main(args: Array[String]): Unit = {
    // val filename = "Day2-sample.txt"
    // val filename = "Day2-part2-sample.txt"
    val filename = "Day2-input.txt"

    val inputLines: Iterator[String] =
      Source.fromResource(filename).getLines

    val rows =
      inputLines.map(r => r.split(' ').map(s => s.toInt).toVector).toVector

    val part1Result = rows.foldLeft(0)((total, row) => {
      val diff = row.max - row.min
      total + diff
    })

    println(part1Result)

    val part2Result = rows.foldLeft(0)((total, row) => {
      val allMatches = for {
        x <- row
        y <- row
      } yield (x, y)
      println(allMatches)
      val divider = allMatches.filter { case (x, y) =>
        x != y && x % y == 0
      }.head
      println(divider)
      val result = divider match {
        case (x, y) => x / y
      }

      total + result
    })

    println(part2Result)
  }
}
