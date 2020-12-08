package aoc2020

object Day1 extends Day(1) {
  def sum2020(numbers: List[Int], combinations: Int): Option[Int] = {
    numbers
      .to(LazyList)
      .combinations(combinations)
      .collectFirst {
        case list if list.sum == 2020 => list.product
      }
  }

  override def solution(lines: List[String]): Unit = {
    sum2020(lines.map(_.toInt), 2).foreach(println)
    sum2020(lines.map(_.toInt), 3).foreach(println)
  }
}
