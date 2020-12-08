package aoc2020

import scala.util.matching.Regex

object Day2 {
  private val LOWER = "lower"
  private val UPPER = "upper"
  private val CHARACTER = "character"
  private val PASSWORD = "password"
  val passwordRegex: Regex = "^([0-9]*)-([0-9]*) ([a-z]): (.*)$".r(LOWER, UPPER, CHARACTER, PASSWORD)

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
    passwordRegex.findAllIn(input).matchData.toList.headOption.flatMap { matches =>
      if (matches.groupCount == 4) {
        Some(
          PasswordLine(
            matches.group(LOWER).toInt,
            matches.group(UPPER).toInt,
            matches.group(CHARACTER).toCharArray.head,
            matches.group(PASSWORD)
          )
        )
      } else None
    }
  }

  def validatePasswords(input: Seq[String], f: PasswordLine => Boolean): Int = input.flatMap(parsePasswordLine).count(f)

  def main(args: Array[String]): Unit = {
    Util.processInput("day2") { lines =>
      println(s"Valid passwords part one: ${validatePasswords(lines, _.validatePartOne)}")
      println(s"Valid passwords part two: ${validatePasswords(lines, _.validatePartTwo)}")
    }
  }
}
