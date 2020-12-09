package aoc2020

import scala.annotation.tailrec

object Day9 extends Day(9) {
  def processNumbers(lines: List[Long], preambleSize: Int): Option[Long] = {
    lines
      .to(LazyList)
      .zipWithIndex
      .collectFirst {
        case (value, index)
            if index > preambleSize && !lines
              .slice(index - preambleSize, index)
              .to(LazyList)
              .combinations(2)
              .exists(_.sum == value) =>
          value
      }
  }

  def findWeakness(lines: List[Long], invalidNumber: Long): Option[Long] = {
    @tailrec
    def calculateWeakness(slice: List[Long], processed: List[Long] = List.empty): Option[List[Long]] = {
      slice match {
        case Nil => None
        case head :: tail =>
          processed :+ head match {
            case list if list.sum == invalidNumber => Some(list)
            case list if list.sum < invalidNumber  => calculateWeakness(tail, list)
            case _                                 => None
          }
      }
    }
    lines.indices
      .to(LazyList)
      .map(index => index -> calculateWeakness(lines.slice(index, lines.size)))
      .collectFirst {
        case (_, Some(weaknessList)) => weaknessList
      }
      .map(weakness => weakness.min + weakness.max)
  }

  override def solution(lines: List[String]): Unit = {
    val numbers = lines.map(_.toLong)
    processNumbers(numbers, 25).foreach { value =>
      println(s"First invalid number is $value")
      findWeakness(numbers, value).foreach(weakness => println(s"Weakness result is $weakness"))
    }
  }
}
