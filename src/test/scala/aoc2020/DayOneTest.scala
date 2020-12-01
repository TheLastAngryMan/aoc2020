package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class DayOneTest extends AnyFunSpec with should.Matchers {
  describe("DayOne:sum2020") {
    it("should correctly work on given example for part one") {
      DayOne.sum2020(List(1721, 979, 366, 299, 675, 1456), 2) should contain(514579)
    }
    it ("should correctly work on given example for part two") {
      DayOne.sum2020(List(1721, 979, 366, 299, 675, 1456), 3) should contain(241861950)
    }
  }
}
