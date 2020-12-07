package aoc2020

import scala.io.Source

object Util {
  def processInput(dayName: String)(f: List[String] => Unit): Unit = {
    val dayFile = Source.fromFile(s"inputs/$dayName.txt")
    val dayInput = dayFile.getLines.toList
    f(dayInput)
    dayFile.close()
  }
}
