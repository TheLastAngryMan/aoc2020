package aoc2020

import scala.io.Source

abstract class Day(number: Int) {
  def main(args: Array[String]): Unit = Util.processInput(s"day$number")(solution)

  def solution(lines: List[String]): Unit
}

object Util {
  def processInput(dayName: String)(f: List[String] => Unit): Unit = {
    val dayFile = Source.fromFile(s"inputs/$dayName.txt")
    val dayInput = dayFile.getLines.toList
    f(dayInput)
    dayFile.close()
  }
}
