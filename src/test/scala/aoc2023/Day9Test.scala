package aoc2023

import aoc2023.Day9.{History, Report}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day9Test extends AnyFunSuite with Matchers {

  test("History.getNextValue should return correct value") {
    History(List(0, 3, 6, 9, 12, 15)).getNextValue shouldBe 18
    History(List(1, 3, 6, 10, 15, 21)).getNextValue shouldBe 28
    History(List(10, 13, 16, 21, 30, 45)).getNextValue shouldBe 68
  }

  test("Report.getSumOfNextValues should return correct value") {
    Report(
      List(
        History(List(0, 3, 6, 9, 12, 15)),
        History(List(1, 3, 6, 10, 15, 21)),
        History(List(10, 13, 16, 21, 30, 45))
      )
    ).getSumOfNextValues shouldBe 114
  }

  test("History.getPrevValue should return correct value") {
    History(List(0, 3, 6, 9, 12, 15)).getPreviousValue shouldBe -3
    History(List(1, 3, 6, 10, 15, 21)).getPreviousValue shouldBe 0
    History(List(10, 13, 16, 21, 30, 45)).getPreviousValue shouldBe 5
  }

  test("Report.getSumOfPreviousValues should return correct value") {
    Report(
      List(
        History(List(0, 3, 6, 9, 12, 15)),
        History(List(1, 3, 6, 10, 15, 21)),
        History(List(10, 13, 16, 21, 30, 45))
      )
    ).getSumOfPreviousValues shouldBe 2
  }

}
