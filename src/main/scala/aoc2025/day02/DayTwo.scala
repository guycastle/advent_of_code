package aoc2025.day02

import utils.DailyChallenge

import java.time.LocalDate
import scala.collection.immutable.NumericRange

object DayTwo extends DailyChallenge[BigInt]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 2)

  override def partOne(input: Seq[String]): BigInt = processInput(input, isValidIdPart1)

  override def partTwo(input: Seq[String]): BigInt = processInput(input, isValidIdPart2)

  @main def run(): Unit = evaluate()

  private type BigRange = NumericRange[Long]

  private def processInput(input: Seq[String], validationFunction: Long => Boolean): BigInt =
    input.headOption.toSeq.flatMap(parseInput).map(sumOfInvalidIds(_, validationFunction)).sum
  end processInput

  def sumOfInvalidIds(range: BigRange, validationFunction: Long => Boolean): BigInt = range.filterNot(validationFunction).sum
  end sumOfInvalidIds

  val isValidIdPart1: Long => Boolean = _.toString match
    case id if id.length % 2 != 0 => true
    case id if id.distinct.length == 1 => false
    case id                            => id.grouped(id.length / 2).toSeq match
        case Seq(first, second) if first == second => false
        case _                                     => true
  end isValidIdPart1

  val isValidIdPart2: Long => Boolean = _.toString match
    case id if id.length > 1 && id.distinct.length == 1 => false
    case id                                             => !(2 to id.length / 2).map(id.grouped).exists(_.toSet.size == 1)
  end isValidIdPart2

  private val parseInput: String => Seq[BigRange] = _.split(",").toIndexedSeq
    .flatMap:
      case s"$start-$end" =>
        for
          s <- start.toLongOption
          e <- end.toLongOption
        yield s to e
      case _ => None

end DayTwo
