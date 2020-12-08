package aoc2020

import scala.annotation.tailrec

object Day5 extends Day(5) {
  @tailrec
  def processBinarySpace(lines: List[Char], lowerBound: Int, upperBound: Int)(implicit boundingChars: (Char, Char)): Int = {
    lines match {
      case Nil => lowerBound
      case head :: tail =>
        val boundDifference = ((upperBound - lowerBound) / 2) + 1
        val newLower = if (head == boundingChars._2) lowerBound + boundDifference else lowerBound
        val newUpper = if (head == boundingChars._1) upperBound - boundDifference else upperBound
        processBinarySpace(tail, newLower, newUpper)
    }
  }
  def processRow(lines: List[Char]): Int = processBinarySpace(lines, 0, 127)('F' -> 'B')
  def processSeat(lines: List[Char]): Int = processBinarySpace(lines, 0, 7)('L' -> 'R')
  def processBoardingPass(lines: List[Char]): Int = processRow(lines.slice(0, 7)) * 8 + processSeat(lines.slice(7, 10))

  def solution(lines: List[String]): Unit = {
    //Part One
    val boardingPassIds = lines.map(bp => processBoardingPass(bp.toList))
    println(s"Max id is ${boardingPassIds.max}")

    //Part Two
    val sortedIds = boardingPassIds.sorted
    val possResult = sortedIds.to(LazyList).zipWithIndex.collectFirst {
      case (value, index) if index > 0 && sortedIds(index - 1) != value - 1 => value
    }
    println(s"Boarding pass id is ${possResult.map(_.toString).getOrElse("Could not find seat")}")
  }
}
