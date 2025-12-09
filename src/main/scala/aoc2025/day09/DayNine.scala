package aoc2025.day09

import utils.DailyChallenge
import utils.structs.Coordinate

import java.time.LocalDate

object DayNine extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 9)

  override def partOne(input: Seq[String]): Long = largestSquareSurfaceFromPoints(parseInput(input))

  override def partTwo(input: Seq[String]): Long = 0

  @main def run(): Unit = evaluate()

  private val largestSquareSurfaceFromPoints: Seq[Coordinate] => Long = _.combinations(2)
    .collect:
      case Seq(a, b) => ((a.x - b.x).abs + 1L) * ((a.y - b.y).abs + 1L)
    .maxOption
    .getOrElse(0L)

  private val parseInput: Seq[String] => Seq[Coordinate] = _.collect:
    case s"$x,$y" =>
      for
        x <- x.toIntOption
        y <- y.toIntOption
      yield Coordinate(x = x, y = y)
  .flatten

end DayNine
