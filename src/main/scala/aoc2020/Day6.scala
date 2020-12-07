package aoc2020

import scala.annotation.tailrec

object Day6 extends App {
  @tailrec
  def processDeclarations(lines: List[String], currentSet: Seq[Set[Char]] = Seq.empty, processedGroups: Seq[Seq[Set[Char]]] = Seq.empty): Seq[Seq[Set[Char]]] = {
    lines match {
      case Nil => processedGroups :+ currentSet
      case head :: tail if head.isEmpty => processDeclarations(tail, Seq.empty, processedGroups :+ currentSet)
      case head :: tail => processDeclarations(tail, currentSet :+ head.toList.toSet, processedGroups)
    }
  }

  def sumOfAnsweredQuestions(lines: List[String]): Int = {
    processDeclarations(lines).map(_.foldLeft(Set.empty[Char])((a,b) => a ++ b).size).sum
  }

  def sumOfAllAnsweredQuestions(lines: List[String]): Int = {
    processDeclarations(lines).map(answers => answers.foldLeft(answers.headOption.getOrElse(Set.empty))((a, b) => a.intersect(b)).size).sum
  }

  Util.processInput("day6") {lines =>
    println(s"Total number of answered questions: ${sumOfAnsweredQuestions(lines)}")
    println(s"Total number of all answered questions: ${sumOfAllAnsweredQuestions(lines)}")
  }
}
