package aoc2025.day04

import utils.DailyChallenge
import utils.Syntax.some

import java.time.LocalDate

object DayFour extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 4)

  override def partOne(input: Seq[String]): Int = countSquaresWithLessThan4AdjacentRollsOfPaper(parseInput(input))
  end partOne

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  enum Move(val move: Coordinate => Coordinate):
    case Up extends Move(move = c => (c.x, c.y - 1))
    case UpRight extends Move(move = c => (c.x + 1, c.y - 1))
    case Right extends Move(move = c => (c.x + 1, c.y))
    case DownRight extends Move(move = c => (c.x + 1, c.y + 1))
    case Down extends Move(move = c => (c.x, c.y + 1))
    case DownLeft extends Move(move = c => (c.x - 1, c.y + 1))
    case Left extends Move(move = c => (c.x - 1, c.y))
    case UpLeft extends Move(move = c => (c.x - 1, c.y - 1))
  end Move

  private type Coordinate = (x: Int, y: Int)

  private type Grid = Map[Int, Map[Int, Char]]

  private val countSquaresWithLessThan4AdjacentRollsOfPaper: Grid => Int = grid =>
    grid
      .map:
        case (y, row) => row.count:
            case (x, _) =>
              val adjacentRollsOfPaper = Move.values.count: move =>
                val (checkX, checkY) = move.move((x, y))
                grid.get(checkY).flatMap(_.get(checkX)).isDefined
              adjacentRollsOfPaper < 4
      .sum
  end countSquaresWithLessThan4AdjacentRollsOfPaper

  private val parseInput: Seq[String] => Grid = _.zipWithIndex
    .map:
      case (row, rowNum) => rowNum -> row.zipWithIndex
          .flatMap:
            case (square, colNum) => square match
                case c if c == '@' => (colNum -> c).some
                case _             => None
          .toMap
    .toMap
  end parseInput

end DayFour
