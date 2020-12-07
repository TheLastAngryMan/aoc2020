package aoc2020

import scala.annotation.tailrec

object DayFive extends App {
  @tailrec
  def processBinarySpace(lines: List[Char], lowerBound: Int, upperBound: Int, lowerHalfCharacter:Char, upperHalfCharacter: Char): Int = {
    lines match {
      case Nil => lowerBound
      case head :: tail =>
        val boundDifference = ((upperBound - lowerBound) / 2) + 1
        val newLower = if (head == upperHalfCharacter) lowerBound + boundDifference else lowerBound
        val newUpper = if (head == lowerHalfCharacter)  upperBound - boundDifference else upperBound
        processBinarySpace(tail, newLower, newUpper, lowerHalfCharacter, upperHalfCharacter)
    }
  }
  def processRow(lines: List[Char]): Int = processBinarySpace(lines, 0, 127, 'F', 'B')
  def processSeat(lines: List[Char]): Int = processBinarySpace(lines, 0,7, 'L', 'R')
  def processBoardingPass(lines: List[Char]): Int = processRow(lines.slice(0, 7)) * 8 + processSeat(lines.slice(7, 10))

  Util.processInput("dayFive") { lines =>
    //Part One
    val boardingPassIds = lines.map(bp => processBoardingPass(bp.toList))
    println(s"Max id is ${boardingPassIds.max}")

    //Part Two
    val sortedIds = boardingPassIds.sorted
    val (nextSeatId, _) = sortedIds.zipWithIndex.filter {
      case (value, index) if index > 0 => sortedIds(index - 1) != value - 1
      case _ => false
    }.head
    println(s"Boarding pass id is ${nextSeatId - 1}")
  }
}
