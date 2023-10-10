package aoc2020

import aoc2020.Day19.Rule.{CharRule, ComboRule, OrRule}

import scala.annotation.tailrec
import scala.io.Source

object Day19 {

  private val filenamePart1Sample = "2020/Day19/part1-sample.txt"
  private val filenamePart1Input = "2020/Day19/part1-input.txt"
  private val filenamePart2SampleUnmodified =
    "2020/Day19/part2-sampleUnmodified.txt"
  private val filenamePart2SampleModified =
    "2020/Day19/part2-sampleModified.txt"

  case class Domain(rules: Map[Int, Rule], messages: List[String])

  case class MatchResult(branchedInputs: List[String])

  sealed trait Rule {
    def matches(inputs: List[String], allRules: Map[Int, Rule]): MatchResult
  }

  object Rule {
    case class CharRule(char: Char) extends Rule {
      override def matches(
          inputs: List[String],
          allRules: Map[Int, Rule]
      ): MatchResult = MatchResult(
        inputs.filter(i => i.nonEmpty && i(0) == char).map(_.tail)
      )
    }

    case class ComboRule(ruleIds: List[Int]) extends Rule {
      override def matches(
          inputs: List[String],
          allRules: Map[Int, Rule]
      ): MatchResult = {
        val rules = ruleIds.map(allRules(_))

        @tailrec
        def checkMatches(
            checkInputs: List[String],
            remainingRules: List[Rule]
        ): MatchResult =
          remainingRules.head.matches(checkInputs, allRules) match {
            case MatchResult(branches) if branches.nonEmpty =>
              if (remainingRules.tail.isEmpty) {
                MatchResult(branches)
              } else {
                checkMatches(branches, remainingRules.tail)
              }
            case _ => MatchResult(List.empty)
          }

        checkMatches(inputs, rules)
      }
    }

    case class OrRule(rules: List[Rule]) extends Rule {
      override def matches(
          inputs: List[String],
          allRules: Map[Int, Rule]
      ): MatchResult = {
        val results = rules.map(_.matches(inputs, allRules))
        MatchResult(results.flatMap(_.branchedInputs))
      }
    }
  }

  private def parseComboRule(ruleString: String): ComboRule = {
    ComboRule(
      ruleString.split(' ').filterNot(s => s.isEmpty).map(_.toInt).toList
    )
  }

  private def parseDomain(filename: String): Domain = {
    val contents = Source.fromResource(filename).mkString
    val sections = contents.split("\n\n")

    val rulesSection = sections(0)
    val rules = rulesSection
      .split('\n')
      .map { r =>
        {
          val parts = r.split(':')
          val id = parts(0).toInt
          val ruleString = parts(1)
          if (ruleString.contains('"')) {
            id -> CharRule(ruleString(2))
          } else if (ruleString.contains('|')) {
            id -> OrRule(ruleString.split('|').map(parseComboRule).toList)
          } else {
            id -> parseComboRule(ruleString)
          }
        }
      }
      .toMap

    val messages = sections(1).split('\n').toList
    Domain(rules, messages)
  }

  private def getCountOfMatchingMessages(filename: String): Int = {
    val domain = parseDomain(filename)
    val zeroRule = domain.rules(0)
    val matches = domain.messages.filter(m => {
      val branches = zeroRule.matches(List(m), domain.rules).branchedInputs
      branches.nonEmpty && !branches.exists(b => b.nonEmpty)
    })
    matches.size
  }

  def main(args: Array[String]): Unit = {
    println(getCountOfMatchingMessages(filenamePart1Sample))
    println(getCountOfMatchingMessages(filenamePart1Input))
    println(getCountOfMatchingMessages(filenamePart2SampleUnmodified))
    println(getCountOfMatchingMessages(filenamePart2SampleModified))
  }

}
