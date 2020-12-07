package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class DaySixTest extends AnyFunSpec with should.Matchers {
  val sampleInput = List(
    "abc",
    "",
    "a",
    "b",
    "c",
    "",
    "ab",
    "ac",
    "",
    "a",
    "a",
    "a",
    "a",
    "",
    "b",
    ""
  )
  describe("DaySix:sumOfAnsweredQuestions") {
    it("should work with sample input") {
      DaySix.sumOfAnsweredQuestions(sampleInput) shouldBe 11
    }
  }
  describe("DaySix:sumOfAllAnsweredQuestions") {
    it ("should work with sample input") {
      DaySix.sumOfAllAnsweredQuestions(sampleInput) shouldBe 6
    }
  }
}
