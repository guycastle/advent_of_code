package aoc2024.day07

import utils.DailyChallenge

import java.time.LocalDate

object DaySeven extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 7)

  override def partOne(input: Seq[String]): Long = parseInput(input)
    .withFilter(canBeSolved(_, Operator.values.filterNot(_ == Operator.concat)))
    .map(_.value)
    .sum

  override def partTwo(input: Seq[String]): Long = parseInput(input)
    .withFilter(canBeSolved(_))
    .map(_.value)
    .sum

  @main def run(): Unit = evaluate()

  private case class Equation(value: Long, numbers: List[Long])

  private enum Operator(val func: (Long, Long) => Long):
    case times extends Operator(func = (a, b) => a * b)
    case plus extends Operator(func = (a, b) => a + b)
    case concat extends Operator(func = (a, b) => s"$a$b".toLong)
  end Operator

  private val parseInput: Seq[String] => Seq[Equation] = _.flatMap:
    case s"$value: $numbers" =>
      value.toLongOption.map(v => Equation(value = v, numbers = numbers.split("\\s").toList.flatMap(_.toLongOption)))
    case _ => None

  private def canBeSolved(equation: Equation, operators: Array[Operator] = Operator.values): Boolean =
    equation.numbers match
      case List(a, b)             => operators.exists(op => op.func(a, b) == equation.value)
      case head :: Nil            => head == equation.value
      case head :: (next :: tail) =>
        operators.exists(op => canBeSolved(equation.copy(numbers = op.func(head, next) +: tail), operators))
      case Nil => false
end DaySeven
