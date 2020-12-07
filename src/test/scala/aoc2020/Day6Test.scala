package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day6Test extends AnyFunSpec with should.Matchers {
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
      Day6.sumOfAnsweredQuestions(sampleInput) shouldBe 11
    }
  }
  describe("DaySix:sumOfAllAnsweredQuestions") {
    it ("should work with sample input") {
      Day6.sumOfAllAnsweredQuestions(sampleInput) shouldBe 6
    }
  }
}
