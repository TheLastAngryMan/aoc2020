package aoc2020

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day7Regexs {
  val initBagRegex: Regex = "^([a-z ]+) bags contain".r
  val listBagRegex: Regex = "([0-9]+) ([a-z ]+) (bag|bags)".r
}

object Day7 extends App {
  case class BagRule(numb: Int, bagType: String)
  @tailrec
  def processRules(rules: List[String], ruleMap: Map[String, Seq[BagRule]] = Map.empty): Map[String, Seq[BagRule]] = {
    rules match {
      case Nil => ruleMap
      case head :: tail =>
        Day7Regexs.initBagRegex.findFirstMatchIn(head) match {
          case Some(containerBagMatch) =>
            val bagRules = Day7Regexs.listBagRegex.findAllMatchIn(head).map(mr => BagRule(mr.group(1).toInt, mr.group(2))).toSeq
            processRules(tail, ruleMap + (containerBagMatch.group(1) -> bagRules))
          case None =>
            println(s"Could not find initial regex in $head")
            processRules(tail, ruleMap)
        }
    }
  }

  @tailrec
  def processReverseRules(rules: List[String], ruleMap: Map[String, Seq[String]] = Map.empty): Map[String, Seq[String]] = {
    rules match {
      case Nil => ruleMap
      case head :: tail =>
        Day7Regexs.initBagRegex.findFirstMatchIn(head) match {
          case Some(containerBagMatch) =>
            val updatedMap = Day7Regexs.listBagRegex.findAllMatchIn(head).foldLeft(ruleMap) {
              case (map, regexMatch) =>
                val mapKey = regexMatch.group(2)
                map + (mapKey -> (map.getOrElse(mapKey, Seq.empty) :+ containerBagMatch.group(1)))
            }
            processReverseRules(tail, updatedMap)
          case None =>
            println(s"Could not find initial regex in $head")
            processReverseRules(tail, ruleMap)
        }
    }
  }

  def traverseMap(inputRule: String, ruleMap: Map[String, Seq[String]]): Set[String] = {
    @tailrec
    def recurseRules(rulesToBeChecked: List[String], validResults: Set[String] = Set.empty): Set[String] = {
      rulesToBeChecked match {
        case Nil => validResults
        case head :: tail =>
          ruleMap.get(head) match {
            case Some(rules) =>
              recurseRules(tail ++ rules.filterNot(validResults.contains).toList, validResults ++ rules)
            case None =>
              println(s"nothing can contain $head")
              recurseRules(tail, validResults)
          }
      }
    }
    recurseRules(List(inputRule))
  }

  def countNumberBags(inputRule: String, ruleMap: Map[String, Seq[BagRule]]): Int = {
    @tailrec
    def recurseBagRules(toBeChecked: List[BagRule], accum: Int = 0): Int = {
      toBeChecked match {
        case Nil => accum
        case head :: tail =>
          ruleMap.get(head.bagType) match {
            case Some(value) =>
              recurseBagRules(
                tail ++ value.map(bagRule => bagRule.copy(numb = bagRule.numb * head.numb)),
                accum + value.map(_.numb).sum * head.numb
              )
            case None =>
              println(s"$head doesn't contain any bags")
              recurseBagRules(tail, accum)
          }
      }
    }
    recurseBagRules(List(BagRule(1, inputRule)))
  }

  def validBagContainers(bagType: String, rules: List[String]): Set[String] = traverseMap(bagType, processReverseRules(rules))

  def maxNumberOfBags(bagType: String, rules: List[String]): Int = countNumberBags(bagType, processRules(rules))

  Util.processInput("day7") { lines =>
    println(s"Number of bags that can contain shiny gold is ${traverseMap("shiny gold", processReverseRules(lines)).size}")
    println(s"Max number of sub-bags inside shiny gold is ${countNumberBags("shiny gold", processRules(lines))}")
  }
}
