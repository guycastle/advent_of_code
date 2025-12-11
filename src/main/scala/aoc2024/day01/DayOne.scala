package aoc2024.day01

import utils.DailyChallenge
import utils.syntax.*

import java.time.LocalDate

object DayOne extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 1)

  override def partOne(input: Seq[String]): Int =
    val groupOne -> groupTwo = parseInputForPartOne(input)
    groupOne.sorted
      .zip(groupTwo.sorted)
      .map:
        case (a -> b) => math.abs(a - b)
      .sum
  end partOne

  override def partTwo(input: Seq[String]): Int =
    val ids -> counts = parseInputForPartTwo(input)
    ids
      .map: id =>
        id * counts.getOrElse(id, 0)
      .sum
  end partTwo

  @main def run(): Unit = evaluate()

  private lazy val parseInputForPartOne: Seq[String] => (Seq[Int], Seq[Int]) =
    _.foldLeft((Seq.empty, Seq.empty)): (acc, line) =>
      val split         = line.split("\\s+")
      val left -> right = acc
      (split.headOption.flatMap(_.toIntOption), split.lift(1).flatMap(_.toIntOption)) match
        case (Some(a), Some(b)) => (left :+ a) -> (right :+ b)
        case _                  => acc
      end match

  private lazy val parseInputForPartTwo: Seq[String] => (Seq[Int], Map[Int, Int]) =
    _.foldLeft((Seq.empty, Map.empty)): (acc, line) =>
      val split         = line.split("\\s+")
      val left -> right = acc
      (split.headOption.flatMap(_.toIntOption), split.lift(1).flatMap(_.toIntOption)) match
        case (Some(a), Some(b)) => (left :+ a) -> right.updatedWith(b)(count => (count.getOrElse(0) + 1).some)
        case _                  => acc
      end match

end DayOne
