package aoc2020

import scala.util.matching.Regex
import cats.syntax.option._

object Day2 extends Day(2) {
  val passwordRegex: Regex = "^([0-9]*)-([0-9]*) ([a-z]): (.*)$".r

  case class PasswordLine(lowerLimit: Int, upperLimit: Int, character: Char, password: String) {
    def validatePartOne: Boolean = {
      val numbRepeats = password.toCharArray.count(_.toLower == character)
      numbRepeats >= lowerLimit && numbRepeats <= upperLimit
    }

    def validatePartTwo: Boolean = {
      val passwordCharacters = password.toList
      passwordCharacters.lift(lowerLimit - 1).contains(character) ^ passwordCharacters.lift(upperLimit - 1).contains(character)
    }
  }

  def parsePasswordLine(input: String): Option[PasswordLine] = {
    input match {
      case passwordRegex(lower, upper, character, password) =>
        PasswordLine(lower.toInt, upper.toInt, character.toCharArray.head, password).some
      case _ => None
    }
  }

  def validatePasswords(input: Seq[String], f: PasswordLine => Boolean): Int = input.flatMap(parsePasswordLine).count(f)

  def solution(lines: List[String]): Unit = {
    println(s"Valid passwords part one: ${validatePasswords(lines, _.validatePartOne)}")
    println(s"Valid passwords part two: ${validatePasswords(lines, _.validatePartTwo)}")
  }
}
