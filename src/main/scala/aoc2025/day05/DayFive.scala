package aoc2025.day05

import utils.DailyChallenge

import java.time.LocalDate
import scala.collection.immutable.NumericRange

object DayFive extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 5)

  override def partOne(input: Seq[String]): Long = countFreshIngredients(parseInput(input))

  override def partTwo(input: Seq[String]): Long = 0
  end partTwo

  @main def run(): Unit = evaluate()

  private type LongRange = NumericRange[Long]

  private type IngredientDatabase = (idRanges: Seq[LongRange], availableIngredients: Seq[Long])

  private val countFreshIngredients: IngredientDatabase => Int = db =>
    db.availableIngredients.count: ingredient =>
      println(s"Counting ingredient $ingredient")
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
