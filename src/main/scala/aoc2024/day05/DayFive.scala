package aoc2024.day05

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec

object DayFive extends DailyChallenge[Int]:
  override lazy val day: LocalDate = LocalDate.of(2024, 12, 5)

  override def partOne(input: Seq[String]): Int =
    val (rules, updates) = parseInput(input)
    updates.foldLeft(0): (sum, update) =>
      if isUpdatedOrderedAccordingToTheRules(update, rules) then sum + middlePageNumber(update) else sum
  end partOne

  override def partTwo(input: Seq[String]): Int =
    val (rules, updates) = parseInput(input)
    updates.foldLeft(0): (sum, update) =>
      if !isUpdatedOrderedAccordingToTheRules(update, rules)
      // sort the rules so they're applied in the right order
      then sum + middlePageNumber(orderUpdate(update, rules.sorted))
      else sum
  end partTwo

  @main def run(): Unit = evaluate()

  private type PageOrderingRule        = (Int, Int)
  private type SafetyManualPagesUpdate = Seq[Int]

  private val parseInput: Seq[String] => (Seq[PageOrderingRule], Seq[SafetyManualPagesUpdate]) = input =>
    val (rules, updates) = input.span(!_.isBlank)
    (
      rules
        .collect:
          case s"$a|$b" => for a <- a.toIntOption; b <- b.toIntOption yield (a, b)
        .flatten,
      updates
        .drop(1) // drop the blank line
        .map(_.split(",").toSeq.flatMap(_.toIntOption)),
    )
  end parseInput

  private val middlePageNumber: SafetyManualPagesUpdate => Int = update => update.lift(update.length / 2).getOrElse(0)

  @tailrec
  private def isUpdatedOrderedAccordingToTheRules(
      update: SafetyManualPagesUpdate,
      rules: Seq[PageOrderingRule]): Boolean =
    if update.isEmpty
    then true
    else
      rules.headOption
        .map((first, second) => (update.indexOf(first), update.indexOf(second))) match
        case Some((-1, _)) | Some((_, -1)) =>
          // rule is not applicable to update, move on to the next one
          if rules.size == 1 then true else isUpdatedOrderedAccordingToTheRules(update, rules.tail)
        case Some((firstIndex, secondIndex)) if rules.size == 1          => firstIndex < secondIndex
        case Some((firstIndex, secondIndex)) if firstIndex < secondIndex =>
          isUpdatedOrderedAccordingToTheRules(update, rules.tail)
        case _ => false
  end isUpdatedOrderedAccordingToTheRules

  private def orderUpdate(update: SafetyManualPagesUpdate, rules: Seq[PageOrderingRule]): SafetyManualPagesUpdate =
    if update.isEmpty
    then update
    else
      rules.foldLeft(update):
        case (ordered, (first, second)) if ordered.contains(first) && ordered.contains(second) =>
          val (indexOfFirst, indexOfSecond) = (ordered.indexOf(first), ordered.indexOf(second))
          if indexOfFirst > indexOfSecond
          then ordered.patch(indexOfFirst, Nil, 1).patch(indexOfSecond, Seq(first, second), 1)
          else ordered
          end if
        case (ordered, _) => ordered
  end orderUpdate

end DayFive
