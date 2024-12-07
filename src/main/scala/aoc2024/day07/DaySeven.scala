package aoc2024.day07

import utils.DailyChallenge

import java.time.LocalDate

object DaySeven extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 7)

  override def partOne(input: Seq[String]): Long = parseInput(input).withFilter(canBeSolved).map(_.value).sum

  override def partTwo(input: Seq[String]): Long = 0

  @main def run(): Unit = evaluate()

  private case class Equation(value: Long, numbers: List[Long])

  private enum Operator(val func: (Long, Long) => Long):
    case times extends Operator(func = (a, b) => a * b)
    case plus extends Operator(func = (a, b) => a + b)
  end Operator

  private val parseInput: Seq[String] => Seq[Equation] = _.flatMap:
    case s"$value: $numbers" => value.toLongOption.map(v => Equation(value = v, numbers = numbers.split("\\s").toList.flatMap(_.toLongOption)))
    case _                   => None

  private def canBeSolved(equation: Equation): Boolean =
    equation.numbers match
      case List(a, b)             => Operator.values.exists(op => op.func(a, b) == equation.value)
      case head :: Nil            => head == equation.value
      case head :: (next :: tail) => Operator.values.exists(op => canBeSolved(equation.copy(numbers = op.func(head, next) +: tail)))
      case Nil                    => false
end DaySeven
