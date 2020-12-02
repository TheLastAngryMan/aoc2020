package aoc2020

import aoc2020.DayTwo.PasswordLine
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class DayTwoTest extends AnyFunSpec with should.Matchers {
  describe("DayTwo:parsePasswordLine") {
    it("should work with sample input") {
      DayTwo.parsePasswordLine("1-3 a: abcde") should contain(PasswordLine(1, 3, 'a', "abcde"))
      DayTwo.parsePasswordLine("1-3 b: cdefg") should contain(PasswordLine(1, 3, 'b', "cdefg"))
      DayTwo.parsePasswordLine("2-9 c: ccccccccc") should contain(PasswordLine(2, 9, 'c', "ccccccccc"))
    }
    it("should fail on invalid input") {
      DayTwo.parsePasswordLine("11a-3 a: abcde") shouldBe empty
      DayTwo.parsePasswordLine("1-33f a: abcde") shouldBe empty
    }
  }
  describe("DayTwo:validatePassword") {
    it("should work with sample input for part one") {
      val sampleInput = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
      DayTwo.validatePasswords(sampleInput, _.validatePartOne) shouldBe 2
    }
    it("should work with sample input for part two") {
      val sampleInput = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
      DayTwo.validatePasswords(sampleInput, _.validatePartTwo) shouldBe 1
    }
  }
}
