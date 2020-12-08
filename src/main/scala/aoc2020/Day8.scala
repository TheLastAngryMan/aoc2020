package aoc2020

import scala.annotation.tailrec
import scala.util.matching.Regex
import cats.syntax.option._

object Day8 extends Day(8) {
  val instructionRegex: Regex = "^(nop|acc|jmp) ([+\\-][0-9]+)$".r
  sealed trait Instruction {
    def execute(state: ProgramState): ProgramState
  }
  case class Nop(amount: Int) extends Instruction {
    override def execute(state: ProgramState): ProgramState = state.copyAndIncrement()
  }
  case class Acc(inc: Int) extends Instruction {
    override def execute(state: ProgramState): ProgramState = state.copyAndIncrement(accumulator = state.accumulator + inc)
  }
  case class Jump(amount: Int) extends Instruction {
    override def execute(state: ProgramState): ProgramState = state.copyAndIncrement(pointer = state.pointer + amount)
  }

  def parseInstruction(line: String): Option[Instruction] = {
    line match {
      case instructionRegex(instruction, amount) =>
        instruction match {
          case "nop" => Nop(amount.toInt).some
          case "jmp" => Jump(amount.toInt).some
          case "acc" => Acc(amount.toInt).some
        }
      case _ => None
    }
  }

  case class ProgramState(
      pointer: Int = 0,
      accumulator: Int = 0,
      executedInstructions: Set[Int] = Set.empty,
      programTerminated: Boolean = false
  ) {
    def copyAndIncrement(pointer: Int = this.pointer + 1, accumulator: Int = this.accumulator): ProgramState = {
      this.copy(pointer = pointer, accumulator = accumulator, executedInstructions = this.executedInstructions + this.pointer)
    }
  }

  @tailrec
  def followInstructions(state: ProgramState = ProgramState())(implicit instructions: List[Instruction]): ProgramState = {
    if (!state.executedInstructions.contains(state.pointer)) {
      instructions.lift(state.pointer) match {
        case Some(instruction) => followInstructions(instruction.execute(state))
        case None              => state.copy(programTerminated = true)
      }
    } else state
  }

  def parseAndExecute(input: List[String]): ProgramState = followInstructions()(input.flatMap(parseInstruction))

  def findReplacement(input: List[Instruction]): Option[ProgramState] = {
    //BruteForce Replacement but use lazy evaluation so that we break on the first instance
    input
      .to(LazyList)
      .filter {
        case Nop(_) | Jump(_) => true
        case _                          => false
      }
      .zipWithIndex
      .map {
        case (Nop(amount), index)  => followInstructions()(input.updated(index, Jump(amount)))
        case (Jump(amount), index) => followInstructions()(input.updated(index, Nop(amount)))
        case _                     => ProgramState()
      }
      .collectFirst {
        case state @ ProgramState(_, _, _, programTerminated) if programTerminated => state
      }
  }

  def parseAndFix(input: List[String]): Option[ProgramState] = findReplacement(input.flatMap(parseInstruction))

  def solution(lines: List[String]): Unit = {
    println(s"Final accum value is ${parseAndExecute(lines).accumulator}")
    println(
      s"Accumulator value after fix is ${parseAndFix(lines).map(_.accumulator.toString).getOrElse("could not be detected")}"
    )
  }
}
