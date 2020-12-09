package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day9Test extends AnyFunSpec with should.Matchers {
  val sampleInput: List[Long] = List[Long](
    35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576
  )
  describe("Day9:processNumbers") {
    it("should work with sample input") {
      Day9.processNumbers(sampleInput, 5) should contain(127)
    }
  }
  describe("Day9:findWeakness") {
    it("should work with the sample input") {
      Day9.findWeakness(sampleInput, 127) should contain(62)
    }
  }
}
