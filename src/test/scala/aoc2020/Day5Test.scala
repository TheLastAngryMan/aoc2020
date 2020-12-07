package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day5Test extends AnyFunSpec with should.Matchers {
  describe("DayFive:processBoardingPass") {
    it ("should work with first example") {
      Day5.processBoardingPass("FBFBBFFRLR".toList) shouldBe 357
    }
    it ("should work with second example") {
      Day5.processBoardingPass("BFFFBBFRRR".toList) shouldBe 567
    }
    it ("should work with third example") {
      Day5.processBoardingPass("FFFBBBFRRR".toList) shouldBe 119
    }
    it ("should work with fourth example") {
      Day5.processBoardingPass("BBFFBBFRLL".toList) shouldBe 820
    }
    it ("should work with 0 row") {
      Day5.processBoardingPass("FFFFFFFLLL".toList) shouldBe 0
    }
    it ("should work with end row") {
      Day5.processBoardingPass("BBBBBBBRRR".toList) shouldBe 127 * 8 + 7
    }
  }
}
