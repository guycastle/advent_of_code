package aoc2024.day02

import utils.DailyChallenge
import utils.Syntax._

import java.time.LocalDate

object DayTwo extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 2)

  override def partOne(input: Seq[String]): Int = parseInput(input).count(reportIsSafe)

  override def partTwo(input: Seq[String]): Int = parseInput(input).count(reportIsSafeUsingDampener)

  @main def run(): Unit = evaluate()

  private type Level  = Int
  private type Report = Seq[Level]

  private val parseInput: Seq[String] => Seq[Report] = _.map(_.toIntSequence)

  private val reportBecomesUnsafeAtIndex: Report => Option[Int] = report =>
    if report.size <= 1 then None
    else
      (report.head, report(1)) match
        case (first, second) if first == second => 1.some
        case (first, second) =>
          lazy val validDiffs                   = 1 to 3
          val comparison: (Int, Int) => Boolean = if first > second then (_ > _) else (_ < _)
          report.zipWithIndex
            .sliding(2)
            .takeWhile:
              case (Seq((previous, _), (current, _))) =>
                comparison(previous, current) && validDiffs.contains(math.abs(previous - current))
            .toSeq
            .lastOption match
            case None                                                 => 1.some
            case Some(Seq(_, (_, index))) if index != report.size - 1 => (index + 1).some
            case _                                                    => None

  private val reportIsSafe: Report => Boolean = reportBecomesUnsafeAtIndex(_).isEmpty

  private val reportIsSafeUsingDampener: Report => Boolean = report =>
    reportBecomesUnsafeAtIndex(report) match
      case Some(failureIndex) =>
        // if the report becomes unsafe at the third level, it's also possible to fix it by removing the first element
        val startAt = if failureIndex == 2 then 0 else failureIndex - 1
        // otherwise, check if removing either the failure index or the one just before dampens the error
        (startAt to failureIndex).exists: levelToRemove =>
          reportBecomesUnsafeAtIndex(report.patch(levelToRemove, Nil, 1)).isEmpty
      case None => true

end DayTwo
