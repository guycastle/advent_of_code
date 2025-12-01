package aoc2025.day01

import utils.DailyChallenge
import utils.Syntax.*

import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object DayOne extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 1)

  override def partOne(input: Seq[String]): Int =
    parseInput(input) match
      case Failure(ex) => throw ex
      case Success(rotations) =>
        val (_, zeroes) = rotations.foldLeft((50, 0)):
          case ((position, zeroes), rotation) =>
            val newPosition = rotate(currentPosition = position, rotation = rotation)
            if newPosition == 0 then (newPosition, zeroes + 1) else (newPosition, zeroes)
        zeroes
    end match
  end partOne

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  enum Direction:
    case Left, Right
  end Direction

  case class Rotation(direction: Direction, distance: Int)
  object Rotation:
    def left(distance: Int): Rotation = Rotation(Direction.Left, distance)
    def right(distance: Int): Rotation = Rotation(Direction.Right, distance)
  end Rotation

  private def rotate(currentPosition: Int = 50, rotation: Rotation): Int = {
    rotation.direction match
      case Direction.Left =>
        val newPosition = currentPosition - (rotation.distance % 100)
        if newPosition < 0 then 100 + newPosition else newPosition
      case Direction.Right =>
        val newPosition = currentPosition + (rotation.distance % 100)
        if newPosition > 99 then newPosition - 100 else newPosition
    end match
  }
  end rotate

  lazy val parseInput: Seq[String] => Try[Seq[Rotation]] = _.toTryIterable:
    case s"R$distance" => distance.toIntOption.map(Rotation.right).toTry
    case s"L$distance" => distance.toIntOption.map(Rotation.left).toTry
    case other => Failure(new IllegalArgumentException(s"Invalid rotation: $other"))


end DayOne
