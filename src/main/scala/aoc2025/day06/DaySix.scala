package aoc2025.day06

import utils.DailyChallenge

import java.time.LocalDate

object DaySix extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 6)

  override def partOne(input: Seq[String]): Long = parseInput(input).map(_.solve).sum

  override def partTwo(input: Seq[String]): Long = 0

  @main def run(): Unit = evaluate()

  type Problem = (numbers: Seq[Long], function: (Long, Long) => Long)
  extension (p: Problem) def solve: Long = p.numbers.reduce(p.function)
  end extension

  private val parseInput: Seq[String] => Seq[Problem] = lines =>
    lines
      .map(_.split("\\s+").toSeq)
      .transpose
      .flatMap: problemParts =>
        val numbers = problemParts.dropRight(1).flatMap(_.toLongOption)
        problemParts.lastOption.collect:
          case "+" => (numbers = numbers, function = _ + _)
          case "*" => (numbers = numbers, function = _ * _)

end DaySix
