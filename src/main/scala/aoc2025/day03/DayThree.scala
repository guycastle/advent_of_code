package aoc2025.day03

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec

object DayThree extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 3)

  override def partOne(input: Seq[String]): Long = parseInput(input).map(bank => findLargestJoltage(bank, 2)).sum

  override def partTwo(input: Seq[String]): Long = parseInput(input).map(bank => findLargestJoltage(bank, 12)).sum

  @main def run(): Unit = evaluate()

  private type JoltageRating = Int
  private type BatteryBank   = Seq[JoltageRating]
  private type BankJoltage   = Long

  @tailrec
  private def findLargestJoltage(
      bank: BatteryBank,
      requiredAmountOfBatteries: Int,
      selectedBatteryJoltageRatings: Seq[JoltageRating] = Seq.empty): BankJoltage =
    if requiredAmountOfBatteries == 0
    then
      selectedBatteryJoltageRatings.zipWithIndex
        .map:
          // multiply the battery's joltage rating by 10 to the power of the reversed index
          case (joltageRating, idx) =>
            math.pow(10, selectedBatteryJoltageRatings.length - 1 - idx).toLong * joltageRating
        .sum
    else
      val largestBatteryJoltage = bank.dropRight(requiredAmountOfBatteries - 1).max
      findLargestJoltage(
        bank = bank.slice(bank.indexOf(largestBatteryJoltage) + 1, bank.length),
        requiredAmountOfBatteries = requiredAmountOfBatteries - 1,
        selectedBatteryJoltageRatings = selectedBatteryJoltageRatings :+ largestBatteryJoltage,
      )
  end findLargestJoltage

  private val parseInput: Seq[String] => Seq[BatteryBank] = _.map(_.flatMap(_.toString.toIntOption))

end DayThree
