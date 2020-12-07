package aoc2020

object Day1 extends App {
  def sum2020(numbers: List[Int], combinations: Int): Option[Int] = {
    numbers
      .combinations(combinations)
      .collect {
        case list if list.sum == 2020 => list.product
      }
      .toList
      .headOption
  }

  Util.processInput("day1") { lines =>
    sum2020(lines.map(_.toInt), 2).foreach(println)
    sum2020(lines.map(_.toInt), 3).foreach(println)
  }
}
