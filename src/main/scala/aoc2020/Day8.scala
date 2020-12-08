package aoc2020

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day8Regex {
  val instructionKey = "instruction"
  val amountKey = "amount"
  val instructionRegex: Regex = "^(nop|acc|jmp) ([+\\-][0-9]+)$".r(instructionKey, amountKey)
}
object Day8 extends App {
  sealed trait Instruction
  case class Nop(amount: Int) extends Instruction
  case class Acc(inc: Int) extends Instruction
  case class Jump(amount: Int) extends Instruction

  def parseInstruction(line: String): Option[Instruction] = {
    Day8Regex.instructionRegex.findFirstMatchIn(line).map { regexMatch =>
      regexMatch.group(Day8Regex.instructionKey) match {
        case "nop" => Nop(regexMatch.group(Day8Regex.amountKey).toInt)
        case "jmp" => Jump(regexMatch.group(Day8Regex.amountKey).toInt)
        case "acc" => Acc(regexMatch.group(Day8Regex.amountKey).toInt)
      }
    }
  }
  case class ProgramState(
      pointer: Int = 0,
      accumulator: Int = 0,
      executedInstructions: Set[Int] = Set.empty,
      programTerminated: Boolean = false
  ) {
    def copyAndIncrement(pointer: Int = this.pointer, accumulator: Int = this.accumulator): ProgramState = {
      this.copy(pointer = pointer, accumulator = accumulator, executedInstructions = this.executedInstructions + this.pointer)
    }
  }

  @tailrec
  def followInstructions(instructions: List[Instruction], state: ProgramState = ProgramState()): ProgramState = {
    if (!state.executedInstructions.contains(state.pointer)) {
      instructions.lift(state.pointer) match {
        case Some(Nop(_))       => followInstructions(instructions, state.copyAndIncrement(pointer = state.pointer + 1))
        case Some(Jump(amount)) => followInstructions(instructions, state.copyAndIncrement(pointer = state.pointer + amount))
        case Some(Acc(inc)) =>
          followInstructions(
            instructions,
            state.copyAndIncrement(pointer = state.pointer + 1, accumulator = state.accumulator + inc)
          )
        case None =>
          println("Program has reached termination")
          state.copy(programTerminated = true)
      }
    } else state
  }

  def parseAndExecute(input: List[String]): ProgramState = followInstructions(input.flatMap(parseInstruction))

  def findReplacement(input: List[Instruction]): Option[ProgramState] = {
    //BruteForce Replacement but use lazy evaluation so that we break on the first instance
    input
      .to(LazyList)
      .zipWithIndex
      .filter {
        case (Nop(_), _) | (Jump(_), _) => true
        case _                          => false
      }
      .map {
        case (Nop(amount), index)  => followInstructions(input.updated(index, Jump(amount)))
        case (Jump(amount), index) => followInstructions(input.updated(index, Nop(amount)))
        case _                     => ProgramState()
      }
      .collectFirst {
        case state @ ProgramState(_, _, _, programTerminated) if programTerminated => state
      }
  }

  def parseAndFix(input: List[String]): Option[ProgramState] = findReplacement(input.flatMap(parseInstruction))

  Util.processInput("day8") { lines =>
    println(s"Final accum value is ${parseAndExecute(lines).accumulator}")
    println(
      s"Accumulator value after fix is ${parseAndFix(lines).map(_.accumulator.toString).getOrElse("could not be detected")}"
    )
  }
}
