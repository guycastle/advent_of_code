package aoc2024.day03

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec
import scala.util.matching.Regex

object DayThree extends DailyChallenge[Int]:

  val mulInstructionRegex: Regex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)".r

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 3)

  override def partOne(input: Seq[String]): Int = processMemoryByIgnoringCorruptedInstructions(input.mkString)

  override def partTwo(input: Seq[String]): Int =
    processMemoryByIgnoringCorruptedInstructionsWithConditionalStatements(input.mkString)

  @main def run(): Unit = evaluate()

  private val executeMulInstruction: String => Option[Int] =
    case s"mul($a,$b)" =>
      for {
        a <- a.toIntOption
        b <- b.toIntOption
      } yield a * b
    case _ => None

  private val executeAndTallyInstructions: Seq[String] => Int = _.foldLeft(0): (total, instruction) =>
    total + executeMulInstruction(instruction).getOrElse(0)

  private val processMemoryByIgnoringCorruptedInstructions: String => Int =
    memory => executeAndTallyInstructions(mulInstructionRegex.findAllIn(memory).toSeq)

  private enum ConditionalInstruction(val instruction: String):
    case Enable extends ConditionalInstruction("do()")
    case Disable extends ConditionalInstruction("don't()")
  end ConditionalInstruction

  private val processMemoryByIgnoringCorruptedInstructionsWithConditionalStatements: String => Int = memory =>
    @tailrec
    def loop(
        enabledInstructions: Seq[String] = Seq.empty,
        string: String = memory,
        nextInstruction: ConditionalInstruction = ConditionalInstruction.Disable,
    ): Seq[String] =
      string.indexOf(nextInstruction.instruction) match
        case -1 if nextInstruction == ConditionalInstruction.Enable => enabledInstructions
        case -1 if nextInstruction == ConditionalInstruction.Disable =>
          enabledInstructions ++ mulInstructionRegex.findAllIn(string)
        case other if nextInstruction == ConditionalInstruction.Enable =>
          loop(
            enabledInstructions = enabledInstructions,
            string = string.drop(other),
            nextInstruction = ConditionalInstruction.Disable,
          )
        case other if nextInstruction == ConditionalInstruction.Disable =>
          loop(
            enabledInstructions = enabledInstructions ++ mulInstructionRegex.findAllIn(string.take(other)),
            string = string.drop(other),
            nextInstruction = ConditionalInstruction.Enable,
          )
    executeAndTallyInstructions(loop())

end DayThree
