package aoc2024.day11

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec

object DayEleven extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 11)

  override def partOne(input: Seq[String]): Int =
    println(parseInput(input))
    blink(
      rowOfStones = parseInput(input),
      numberOfBlinks = 25,
    ).size

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  private val parseInput: Seq[String] => Seq[Long] = _.flatMap(_.split("\\s+").flatMap(_.toLongOption))

  private type Stone       = Long // Int not sufficient for actual puzzle input
  private type RowOfStones = Seq[Stone]
  private type Rule        = PartialFunction[Stone, RowOfStones]

  private final val rule1: Rule =
    case 0 => Seq(1)

  private final val rule2: Rule =
    case x if x.toString.length % 2 == 0 =>
      val str = x.toString
      str.grouped(str.length / 2).toSeq.flatMap(_.toLongOption)

  private final val rule3: Rule =
    case x => Seq(x * 2024)

  private final val rules = Seq(rule1, rule2)

  @tailrec
  def blink(rowOfStones: RowOfStones, numberOfBlinks: Int): RowOfStones =
    if numberOfBlinks == 0 then rowOfStones
    else
      blink(
        rowOfStones = rowOfStones.flatMap: stone =>
          rules.find(_.isDefinedAt(stone)) match
            case Some(rule) => rule(stone)
            case None       => rule3(stone)
        ,
        numberOfBlinks = numberOfBlinks - 1,
      )
  end blink

end DayEleven
