package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day8Test extends AnyFunSpec with should.Matchers {
  val sampleInputOne = List(
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"
  )
  describe("Day8:parseAndExecute") {
    it("should work with sample input") {
      Day8.parseAndExecute(sampleInputOne).accumulator shouldBe 5
    }
  }
  describe("Day8:parseAndFix") {
    it("should work with sample input") {
      Day8.parseAndFix(sampleInputOne).map(_.accumulator) should contain(8)
    }
  }
}
