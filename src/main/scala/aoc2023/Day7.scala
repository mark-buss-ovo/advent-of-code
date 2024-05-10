package aoc2023

import aoc2023.Day7.HandType._

import scala.annotation.tailrec
import scala.io.Source

object Day7 {

  private val filenamePart1Sample = "2023/Day7/part1-sample.txt"
  private val filenamePart1Input = "2023/Day7/part1-input.txt"

  private val cardValuesPart1: Map[Char, Int] = (2 to 9)
    .map(i => s"$i".head -> i)
    .appendedAll(Map('T' -> 10, 'J' -> 11, 'Q' -> 12, 'K' -> 13, 'A' -> 14))
    .toMap

  sealed trait HandType {
    val strength: Int
  }

  object HandType {
    case object FiveOfAKind extends HandType {
      override val strength: Int = 7
    }
    case object FourOfAKind extends HandType {
      override val strength: Int = 6
    }
    case object FullHouse extends HandType {
      override val strength: Int = 5
    }
    case object ThreeOfAKind extends HandType {
      override val strength: Int = 4
    }
    case object TwoPair extends HandType {
      override val strength: Int = 3
    }
    case object OnePair extends HandType {
      override val strength: Int = 2
    }
    case object HighCard extends HandType {
      override val strength: Int = 1
    }
  }

  sealed trait Hand extends Ordered[Hand] {
    val cards: String

    val `type`: HandType
    protected val cardValues: Map[Char, Int]

    override def compare(that: Hand): Int = {
      val typeComparison = this.`type`.strength.compare(that.`type`.strength)

      if (typeComparison != 0)
        typeComparison
      else {
        @tailrec
        def compareCardValues(nextIndex: Int): Int = {
          val cardComparison = cardValues(this.cards(nextIndex))
            .compare(cardValues(that.cards(nextIndex)))
          if (cardComparison != 0)
            cardComparison
          else
            compareCardValues(nextIndex + 1)
        }

        compareCardValues(0)
      }
    }
  }

  private case class HandPart1(cards: String) extends Hand {
    override val `type`: HandType = {
      val cardCounts = cards.groupBy(c => c).view.mapValues(_.length).toMap

      if (cardCounts.values.exists(_ == 5)) FiveOfAKind
      else if (cardCounts.values.exists(_ == 4)) FourOfAKind
      else if (List(2, 3).forall(cardCounts.values.toList.contains(_)))
        FullHouse
      else if (cardCounts.values.exists(_ == 3)) ThreeOfAKind
      else if (cardCounts.values.count(_ == 2) == 2) TwoPair
      else if (cardCounts.values.exists(_ == 2)) OnePair
      else HighCard
    }

    override protected val cardValues: Map[Char, Int] = cardValuesPart1
  }

  private case class HandBid(hand: Hand, bid: Long)

  private def parseHandBids(filename: String, part2: Boolean): List[HandBid] = {
    Source
      .fromResource(filename)
      .getLines()
      .map { case s"$hand $bid" =>
        HandBid(if (part2) HandPart2(hand) else HandPart1(hand), bid.toLong)
      }
      .toList
  }

  private def getTotalWinnings(filename: String, part2: Boolean): Long = {
    val handBids = parseHandBids(filename, part2)
    val ranked = handBids.sortBy(_.hand)
    ranked.zipWithIndex.map { case (b: HandBid, i) =>
      b.bid * (i + 1)
    }.sum
  }

  private val cardValuesPart2: Map[Char, Int] = cardValuesPart1.updated('J', 1)

  private case class HandPart2(cards: String) extends Hand {
    override val `type`: HandType = {
      val cardCounts = cards.groupBy(c => c).view.mapValues(_.length).toMap
      val (jokers, nonJokers) = cardCounts.partition { case (c, _) =>
        c == 'J'
      }
      val jokerCount =
        jokers.headOption.map { case (_, count) => count }.getOrElse(0)

      if (
        jokerCount == 5 || nonJokers.values
          .exists(c => c == 5 || c + jokerCount == 5)
      )
        FiveOfAKind
      else if (
        jokerCount == 4 || nonJokers.values
          .exists(c => c == 4 || c + jokerCount == 4)
      ) FourOfAKind
      else if (
        List(2, 3).forall(
          nonJokers.values.toList.contains(_)
        ) || (jokerCount == 1 && nonJokers.values.forall(_ == 2))
      )
        FullHouse
      else if (nonJokers.values.exists(c => c == 3 || c + jokerCount == 3))
        ThreeOfAKind
      else if (nonJokers.values.count(_ == 2) == 2) TwoPair
      else if (nonJokers.values.exists(_ == 2) || jokerCount == 1) OnePair
      else HighCard
    }

    override protected val cardValues: Map[Char, Int] = cardValuesPart2
  }

  def main(args: Array[String]): Unit = {
    println(
      s"Part 1 Sample: ${getTotalWinnings(filenamePart1Sample, part2 = false)}"
    )
    println(
      s"Part 1 Input: ${getTotalWinnings(filenamePart1Input, part2 = false)}"
    )
    println(
      s"Part 2 Sample: ${getTotalWinnings(filenamePart1Sample, part2 = true)}"
    )
    println(
      s"Part 2 Input: ${getTotalWinnings(filenamePart1Input, part2 = true)}"
    )
  }

}
