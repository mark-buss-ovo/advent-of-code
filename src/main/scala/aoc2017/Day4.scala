package aoc

import scala.io.Source

object Day4 {

  def countValid(passPhrases: Vector[String]): Int = {
    passPhrases.foldLeft(0)((total, passPhrase) => {
      if (
        passPhrase.split(' ').groupBy(w => w).exists { case (_, list) =>
          list.length > 1
        }
      ) total
      else total + 1
    })
  }

  def countValidPart2(passPhrases: Vector[String]): Int = {
    val valids = passPhrases.filter(passPhrase => {

      val words = passPhrase.split(' ')

      val duplicates = words.groupBy(w => w).exists { case (_, list) =>
        list.length > 1
      }

      val anagrams = words.exists(w => {
        val thisWordGrouped = w.groupBy(c => c)
        val exists = words.exists(o =>
          o != w && o.length == w.length && w.forall(c => o.contains(c)) && o
            .groupBy(c => c) == thisWordGrouped
        )

        exists
      })

      !duplicates && !anagrams
    })

    valids.length
  }

  def main(args: Array[String]): Unit = {
//    val filename = "Day4-sample.txt"
    val filename = "Day4-input.txt"
//    val filename = "Day4-part2-sample.txt"
    val passPhrases: Vector[String] =
      Source.fromResource(filename).getLines.toVector

//    println(countValid(passPhrases))
    println(countValidPart2(passPhrases))
  }
}
