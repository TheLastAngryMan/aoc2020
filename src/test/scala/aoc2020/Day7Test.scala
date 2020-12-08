package aoc2020

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day7Test extends AnyFunSpec with should.Matchers {
  private val ShinyGold = "shiny gold"
  val sampleInput = List(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 " + ShinyGold + " bag.",
    "muted yellow bags contain 2 " + ShinyGold + " bags, 9 faded blue bags.",
    ShinyGold + " bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  )
  describe("Day7:validBagContainers") {
    it("should work with sample input") {
      Day7.traverseMap(ShinyGold, Day7.reverseRuleMap(Day7.processRules(sampleInput))).size shouldBe 4
    }
  }
  describe("Day7:maxNumberOfBags") {
    it("should work with the sample input") {
      Day7.countNumberBags(ShinyGold, Day7.processRules(sampleInput)) shouldBe 32
    }
    it("should work with the second sample input") {
      val sampleInput2 = List(
        ShinyGold + " bags contain 2 dark red bags.",
        "dark red bags contain 2 dark orange bags.",
        "dark orange bags contain 2 dark yellow bags.",
        "dark yellow bags contain 2 dark green bags.",
        "dark green bags contain 2 dark blue bags.",
        "dark blue bags contain 2 dark violet bags.",
        "dark violet bags contain no other bags."
      )
      Day7.countNumberBags(ShinyGold, Day7.processRules(sampleInput2)) shouldBe 126
    }
  }
}
