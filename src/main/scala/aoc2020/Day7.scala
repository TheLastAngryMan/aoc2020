package aoc2020

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day7 extends Day(7) {
  val initBagRegex: Regex = "^([a-z ]+) bags contain".r.unanchored
  val listBagRegex: Regex = "([0-9]+) ([a-z ]+) (bag|bags)".r

  case class BagRule(numb: Int, bagType: String)
  @tailrec
  def processRules(rules: List[String], ruleMap: Map[String, Seq[BagRule]] = Map.empty): Map[String, Seq[BagRule]] = {
    rules match {
      case Nil => ruleMap
      case head :: tail =>
        head match {
          case initBagRegex(containerBag) =>
            val bagRules = listBagRegex.findAllMatchIn(head).map(mr => BagRule(mr.group(1).toInt, mr.group(2))).toSeq
            processRules(tail, ruleMap + (containerBag -> bagRules))
          case _ =>
            println(s"Could not find initial regex in $head")
            processRules(tail, ruleMap)
        }
    }
  }

  def reverseRuleMap(ruleMap: Map[String, Seq[BagRule]]): Map[String, Seq[String]] = {
    ruleMap.view
      .mapValues(_.map(_.bagType))
      .flatMap {
        case (key, values) => values.map(_ -> key)
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSeq)
      .toMap
  }

  def traverseMap(inputRule: String, ruleMap: Map[String, Seq[String]]): Set[String] = {
    @tailrec
    def recurseRules(rulesToBeChecked: List[String], validResults: Set[String] = Set.empty): Set[String] = {
      rulesToBeChecked match {
        case Nil => validResults
        case head :: tail =>
          ruleMap.get(head) match {
            case Some(rules) => recurseRules(tail ++ rules.filterNot(validResults.contains).toList, validResults ++ rules)
            case None        => recurseRules(tail, validResults)
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
            case None => recurseBagRules(tail, accum)
          }
      }
    }
    recurseBagRules(List(BagRule(1, inputRule)))
  }

  def solution(lines: List[String]): Unit = {
    val ShinyGold = "shiny gold"
    val processedRules = processRules(lines)
    val reverseRules = reverseRuleMap(processedRules)
    println(s"Number of bags that can contain $ShinyGold is ${traverseMap(ShinyGold, reverseRules).size}")
    println(s"Max number of sub-bags inside $ShinyGold is ${countNumberBags(ShinyGold, processedRules)}")
  }
}
