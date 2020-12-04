package aoc2020

import aoc2020.DayThree.{Coord, traverseMap}
import cats.syntax.option._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object DayFour {
  sealed trait PassportField {
    def validate: Boolean
  }
  class YearField(value: String, lower: Int, upper: Int) extends PassportField {
    val fourDigits: Regex = "^[0-9]{4}$".r
    def validate: Boolean = {
      fourDigits.matches(value) && value.toInt >= lower && value.toInt <= upper
    }
  }
  case class BirthYear(birthYear: String) extends YearField(birthYear, 1920, 2002)
  case class IssueYear(issueYear: String) extends YearField(issueYear, 2010, 2020)
  case class ExpirationYear(expirationYear: String) extends YearField(expirationYear, 2020, 2030)
  case class Height(value: String) extends PassportField {
    def heightRegex: Regex = "^([0-9]*)(cm|in)$".r
    def validate: Boolean =
      heightRegex.findAllIn(value).matchData.toList.headOption.map(matches => matches.group(1).toInt -> matches.group(2)).exists {
        case (height, "cm") if height >= 150 && height <= 193 => true
        case (height, "in") if height >= 59 && height <= 76   => true
        case _                                                => false
      }
  }
  class RegexField(value: String, regex: Regex) extends PassportField {
    override def validate: Boolean = regex.matches(value)
  }
  case class HairColour(hairColour: String) extends RegexField(hairColour, "^#[a-f0-9]{6}$".r)
  case class EyeColour(eyeColour: String) extends RegexField(eyeColour, "^(amb|blu|brn|gry|grn|hzl|oth)$".r)
  case class PassportId(passportId: String) extends RegexField(passportId, "^[0-9]{9}$".r)

  case class Passport(
      birthYear: Option[BirthYear] = None,
      issueYear: Option[IssueYear] = None,
      expirationYear: Option[ExpirationYear] = None,
      height: Option[Height] = None,
      hairColour: Option[HairColour] = None,
      eyeColour: Option[EyeColour] = None,
      passportId: Option[PassportId] = None,
      countryId: Option[String] = None
  ) {
    def validateFieldsPresent: Boolean = {
      List(birthYear, issueYear, expirationYear, height, hairColour, eyeColour, passportId).foldLeft(true) {
        case (b, maybeField) => b && maybeField.isDefined
      }
    }

    def validateFields: Boolean = {
      List(birthYear, issueYear, expirationYear, height, hairColour, eyeColour, passportId).foldLeft(true) {
        case (b, Some(passportField)) => b && passportField.validate
        case (_, _)                   => false
      }
    }
  }

  @tailrec
  def processPassportBatch(
      batchLines: List[String],
      combinedOutput: List[Passport] = List.empty,
      currentPassport: Passport = Passport()
  ): List[Passport] = {
    batchLines match {
      case Nil => combinedOutput :+ currentPassport
      case head :: tail =>
        if (head.isEmpty) {
          processPassportBatch(tail, combinedOutput :+ currentPassport)
        } else {
          val accumulatedPassport = head.split(" ").map(_.split(":")).foldLeft(currentPassport) {
            case (accumPassport, Array(fieldId, value)) =>
              fieldId match {
                case "iyr" => accumPassport.copy(issueYear = IssueYear(value).some)
                case "byr" => accumPassport.copy(birthYear = BirthYear(value).some)
                case "eyr" => accumPassport.copy(expirationYear = ExpirationYear(value).some)
                case "hgt" => accumPassport.copy(height = Height(value).some)
                case "hcl" => accumPassport.copy(hairColour = HairColour(value).some)
                case "ecl" => accumPassport.copy(eyeColour = EyeColour(value).some)
                case "pid" => accumPassport.copy(passportId = PassportId(value).some)
                case "cid" => accumPassport.copy(countryId = value.some)
                case _     => accumPassport
              }
            case _ => currentPassport
          }
          processPassportBatch(tail, combinedOutput, accumulatedPassport)
        }
    }
  }

  def validatePassportBatch(batch: List[String], f: Passport => Boolean): Int = processPassportBatch(batch).count(f)

  def main(args: Array[String]): Unit = {
    val dayFourFile = Source.fromFile("inputs/dayFour.txt")
    val dayFourInput = dayFourFile.getLines.toList
    println(s"Number of basic valid passports is ${validatePassportBatch(dayFourInput, _.validateFieldsPresent)}")
    println(s"Number of checked valid passports is ${validatePassportBatch(dayFourInput, _.validateFields)}")
    dayFourFile.close()
  }
}
