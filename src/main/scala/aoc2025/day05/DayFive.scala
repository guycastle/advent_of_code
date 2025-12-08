package aoc2025.day05

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object DayFive extends DailyChallenge[BigInt]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 5)

  override def partOne(input: Seq[String]): BigInt = countFreshIngredients(parseInput(input))

  override def partTwo(input: Seq[String]): BigInt = mergeOverlappingRanges(
    parseInput(input).idRanges.toList.sortBy(_.start),
  )
    .map: range =>
      range.end - range.start + 1
    .sum
  end partTwo

  @main def run(): Unit = evaluate()

  private type LongRange = NumericRange[Long]

  private type IngredientDatabase = (idRanges: Seq[LongRange], availableIngredients: Seq[Long])

  @tailrec
  private def mergeOverlappingRanges(
      ranges: List[LongRange],
      consolidated: Seq[LongRange] = Seq.empty): Seq[LongRange] = ranges match
    case range :: otherRange :: tail if range.start == otherRange.start || otherRange.start <= range.end + 1 =>
      mergeOverlappingRanges((range.start to math.max(range.end, otherRange.end)) +: tail, consolidated)
    case range :: tail => mergeOverlappingRanges(tail, consolidated :+ range)
    case Nil           => consolidated

  private val countFreshIngredients: IngredientDatabase => Int = db =>
    db.availableIngredients.count: ingredient =>
      db.idRanges.exists(range => ingredient >= range.start && ingredient <= range.end)

  private val parseInput: Seq[String] => IngredientDatabase = input =>
    val (idRanges, availableIngredients) = input.span(!_.isBlank)
    (
      idRanges = idRanges.flatMap:
        case s"$start-$end" =>
          for
            s <- start.toLongOption
            e <- end.toLongOption
          yield s to e
        case _ => None,
      availableIngredients = availableIngredients.flatMap(_.toLongOption),
    )

end DayFive
