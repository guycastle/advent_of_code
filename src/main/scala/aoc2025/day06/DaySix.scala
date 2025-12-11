package aoc2025.day06

import utils.DailyChallenge
import utils.syntax.*

import java.time.LocalDate

object DaySix extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 6)

  override def partOne(input: Seq[String]): Long = parseInputPart1(input).map(_.solve).sum

  override def partTwo(input: Seq[String]): Long = parseInputPart2(input).map(_.solve).sum

  @main def run(): Unit = evaluate()

  private type Problem = (numbers: Seq[Long], function: (Long, Long) => Long)
  extension (p: Problem) def solve: Long = p.numbers.reduce(p.function)
  end extension

  private val parseInputPart1: Seq[String] => Seq[Problem] = lines =>
    lines
      .map(_.strip.split("\\s+").toSeq)
      .transpose
      .flatMap: problemParts =>
        val numbers = problemParts.dropRight(1).flatMap(_.toLongOption)
        problemParts.lastOption.collect:
          case "+" => (numbers = numbers, function = _ + _)
          case "*" => (numbers = numbers, function = _ * _)
  end parseInputPart1

  private val parseInputPart2: Seq[String] => Seq[Problem] = lines =>
    lines
      .map(_.split(""))
      .transpose
      .reverse
      .map(_.filterNot(_.isBlank).mkString)
      .splitBy(_.isBlank) // see utils.syntax extension
      .flatMap: problem =>
        val numbers = problem.dropRight(1).flatMap(_.toLongOption)
        problem.lastOption.collect:
          case s"${num}*" => (numbers = numbers ++ num.toLongOption, function = _ * _)
          case s"${num}+" => (numbers = numbers ++ num.toLongOption, function = _ + _)
  end parseInputPart2
end DaySix
