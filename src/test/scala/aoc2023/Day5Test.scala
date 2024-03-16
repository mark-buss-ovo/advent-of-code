package aoc2023

import aoc2023.Day5.Window
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day5Test extends AnyFunSuite with Matchers {

  test("getNextWindows should return empty list if no overlap") {
    val sutWindow = Window(10, 20, 30, 40)
    val nextWindow = sutWindow.getNextWindow(0, 9)
    nextWindow shouldBe None
  }

  test(
    "getNextWindows should return sub-window when overlapping on left"
  ) {
    val sutWindow = Window(10, 20, 30, 40)
    val expectedWindow = (30, 35)
    val nextWindow = sutWindow.getNextWindow(5, 15)
    nextWindow shouldBe Some(expectedWindow)
  }

  test(
    "getNextWindows should return sub-window when overlapping on right"
  ) {
    val sutWindow = Window(10, 20, 30, 40)
    val expectedWindow = (35, 40)
    val nextWindow = sutWindow.getNextWindow(15, 25)
    nextWindow shouldBe Some(expectedWindow)
  }

  test(
    "getNextWindows should return correct window when contained in source window"
  ) {
    val sutWindow = Window(10, 20, 30, 40)
    val expectedWindow = (30, 40)
    val nextWindow = sutWindow.getNextWindow(5, 25)
    nextWindow shouldBe Some(expectedWindow)
  }

  test(
    "getNextWindows should return correct windows when source window completely contained"
  ) {
    val sutWindow = Window(10, 20, 30, 40)
    val expectedWindow = (31, 39)
    val nextWindow = sutWindow.getNextWindow(11, 19)
    nextWindow shouldBe Some(expectedWindow)
  }
}
