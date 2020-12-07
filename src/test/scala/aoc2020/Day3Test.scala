package aoc2020

import aoc2020.Day3.Coord
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day3Test extends AnyFunSpec with should.Matchers {
  val input: List[List[Char]] = List(
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  ).map(_.toList)
  describe("DayThree:traverseMap") {
    it("should work with the sample input") {
      Day3.traverseMap(input, Coord(3, 1)) shouldBe 7
    }
    it("should work with the second sample input") {
      List(
        Coord(1,1),
        Coord(3,1),
        Coord(5,1),
        Coord(7,1),
        Coord(1,2)
      ).map(Day3.traverseMap(input, _)).product shouldBe 336
    }
  }
}
