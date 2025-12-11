package aoc2025.day01

import utils.DailyChallenge
import utils.syntax.*

import java.time.LocalDate
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object DayOne extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 1)

  override def partOne(input: Seq[String]): Int =
    parseInput(input) match
      case Failure(ex)        => throw ex
      case Success(rotations) => processRotations(rotations).count(_.position == 0)
    end match
  end partOne

  override def partTwo(input: Seq[String]): Int =
    parseInput(input) match
      case Failure(ex)        => throw ex
      case Success(rotations) => processRotations(rotations).map(_.completeRotations).sum
    end match
  end partTwo

  @main def run(): Unit = evaluate()

  enum Direction(val move: (Int, Int) => Int):
    case Left extends Direction(move = _ - _)
    case Right extends Direction(move = _ + _)
  end Direction

  case class RotationResult(position: Int, completeRotations: Int)

  case class Rotation(direction: Direction, distance: Int):
    def rotate(currentPosition: Int): RotationResult =
      val (dist, completeRotations) = (distance % 100, distance / 100)
      direction.move(currentPosition, dist) match
        case pos if pos % 100 == 0 && dist != 0 => RotationResult(0, completeRotations + 1)
        case pos if pos < 0 && currentPosition == 0 => RotationResult(100 + pos, completeRotations)
        case pos if pos < 0                         => RotationResult(100 + pos, completeRotations + 1)
        case pos if pos > 100                       => RotationResult(pos - 100, completeRotations + 1)
        case pos                                    => RotationResult(pos, completeRotations)
      end match
    end rotate
  end Rotation
  object Rotation:
    def left(distance: Int): Rotation  = Rotation(Direction.Left, distance)
    def right(distance: Int): Rotation = Rotation(Direction.Right, distance)
  end Rotation

  @tailrec
  private def processRotations(
      rotations: Seq[Rotation],
      position: Int = 50,
      results: Seq[RotationResult] = Seq.empty): Seq[RotationResult] = rotations.headOption match
    case Some(rotation) =>
      val result = rotation.rotate(position)
      processRotations(rotations.tail, result.position, results :+ result)
    case None => results
  end processRotations

  lazy val parseInput: Seq[String] => Try[Seq[Rotation]] = _.toTryIterable:
    case s"R$distance" => distance.toIntOption.map(Rotation.right).toTry
    case s"L$distance" => distance.toIntOption.map(Rotation.left).toTry
    case other         => Failure(new IllegalArgumentException(s"Invalid rotation: $other"))

end DayOne
