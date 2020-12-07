package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class DayFiveTest extends AnyFunSpec with should.Matchers {
  describe("DayFive:processBoardingPass") {
    it ("should work with first example") {
      DayFive.processBoardingPass("FBFBBFFRLR".toList) shouldBe 357
    }
    it ("should work with second example") {
      DayFive.processBoardingPass("BFFFBBFRRR".toList) shouldBe 567
    }
    it ("should work with third example") {
      DayFive.processBoardingPass("FFFBBBFRRR".toList) shouldBe 119
    }
    it ("should work with fourth example") {
      DayFive.processBoardingPass("BBFFBBFRLL".toList) shouldBe 820
    }
    it ("should work with 0 row") {
      DayFive.processBoardingPass("FFFFFFFLLL".toList) shouldBe 0
    }
    it ("should work with end row") {
      DayFive.processBoardingPass("BBBBBBBRRR".toList) shouldBe 127 * 8 + 7
    }
  }
}
