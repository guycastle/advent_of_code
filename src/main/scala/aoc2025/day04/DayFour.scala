package aoc2025.day04

import utils.DailyChallenge
import utils.Syntax.some

import java.time.LocalDate
import scala.annotation.tailrec

object DayFour extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 4)

  override def partOne(input: Seq[String]): Int = getCoordinatesOfSquaresWithLessThan4AdjacentRollsOfPaper(parseInput(input)).size
  end partOne

  override def partTwo(input: Seq[String]): Int = removeRollsOfPapersFromGridRecursively(parseInput(input))
  end partTwo

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

  private val getCoordinatesOfSquaresWithLessThan4AdjacentRollsOfPaper: Grid => Seq[Coordinate] = grid =>
    grid.toSeq
      .flatMap:
        case (y, row) => row.flatMap:
            case (x, _) =>
              val adjacentRollsOfPaper = Move.values.count: move =>
                val (checkX, checkY) = move.move((x, y))
                grid.get(checkY).flatMap(_.get(checkX)).isDefined
              if adjacentRollsOfPaper < 4 then (x, y).some else None
              end if

  end getCoordinatesOfSquaresWithLessThan4AdjacentRollsOfPaper

  @tailrec
  private def removeRollsOfPapersFromGridRecursively(grid: Grid, removed: Int = 0): Int = getCoordinatesOfSquaresWithLessThan4AdjacentRollsOfPaper(grid) match
    case removableCoordinates if removableCoordinates.isEmpty => removed
    case removableCoordinates                                 =>

      val updatedGrid = removableCoordinates
        .groupBy(_.y)
        .foldLeft(grid):
          case (g, (y, columnsToRemove)) => g.updatedWith(y):
              case Some(row) => row.view.filterKeys(col => !columnsToRemove.exists(c => c.x == col)) match
                  case updatedRow if updatedRow.isEmpty => None
                  case updatedRow                       => updatedRow.toMap.some
              case None => None
      removeRollsOfPapersFromGridRecursively(updatedGrid, removed + removableCoordinates.size)

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
