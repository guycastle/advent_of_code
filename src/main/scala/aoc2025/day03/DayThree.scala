package aoc2025.day03

import utils.DailyChallenge

import java.time.LocalDate

object DayThree extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 3)

  override def partOne(input: Seq[String]): Int = parseInput(input).map(findLargestJoltage).sum

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  private type JoltageRating = Int
  extension (joltage: JoltageRating)
    private def combine(otherJoltage: JoltageRating): BankJoltage = (joltage * 10) + otherJoltage
  end extension
  private type BatteryBank = Seq[JoltageRating]
  private type BankJoltage = Int

  private def findLargestJoltage(bank: BatteryBank): BankJoltage =
    val batteryOneJoltage = bank.dropRight(1).max
    val batteryTwoJoltage = bank.slice(bank.indexOf(batteryOneJoltage) + 1, bank.length).max
    batteryOneJoltage.combine(batteryTwoJoltage)
  end findLargestJoltage

  private val parseInput: Seq[String] => Seq[BatteryBank] = _.map(_.flatMap(_.toString.toIntOption))

end DayThree
