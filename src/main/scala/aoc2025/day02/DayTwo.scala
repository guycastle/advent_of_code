package aoc2025.day02

import utils.DailyChallenge

import java.time.LocalDate
import scala.collection.immutable.NumericRange

object DayTwo extends DailyChallenge[BigInt]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 2)

  override def partOne(input: Seq[String]): BigInt = input.headOption.toSeq.flatMap(parseInput).map(sumOfInvalidIds).sum

  override def partTwo(input: Seq[String]): BigInt = 0

  @main def run(): Unit = evaluate()

  private type BigRange = NumericRange[Long]

  lazy val sumOfInvalidIds: BigRange => BigInt = range => range.filterNot(isValidId).sum
  end sumOfInvalidIds

  val isValidId: Long => Boolean = _.toString match
    case id if id.length % 2 != 0 => true
    case id if id.distinct.length == 1 => false
    case id                            => id.grouped(id.length / 2).toSeq match
        case Seq(first, second) if first == second => false
        case _                                     => true
  end isValidId

  lazy val parseInput: String => Seq[BigRange] = _.split(",").toIndexedSeq
    .map(_.strip)
    .flatMap:
      case s"$start-$end" =>
        for
          s <- start.toLongOption
          e <- end.toLongOption
        yield s to e
      case _ => None

end DayTwo
