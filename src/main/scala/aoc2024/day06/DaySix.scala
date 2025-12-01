package aoc2024.day06

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec

object DaySix extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 6)

  override def partOne(input: Seq[String]): Int =
    val (grid, start, direction) = parseInput(input)
    walkGrid(grid = grid, position = start, move = direction, history = Set(start)).size
  end partOne

  override def partTwo(input: Seq[String]): Int =
    val (grid, start, direction) = parseInput(input)
    // not really happy with performance, need to find better way.
    walkGrid(grid = grid, position = start, move = direction, history = Set(start))
      .filterNot(_ == start)
      .count: (newObstructionX, newObstructionY) =>
        isGridLooping(
          grid = grid.updatedWith(newObstructionY)(rowOpt => rowOpt.map(_.updated(newObstructionX, '#'))),
          position = start,
          move = direction,
          history = Set.empty,
        )
  end partTwo

  @main def run(): Unit = evaluate()

  private type Position    = (Int, Int)
  private type Movement    = (Int, Int)
  private type Up          = '^'
  private type Down        = 'v'
  private type Left        = '<'
  private type Right       = '>'
  private type Direction   = Up | Down | Left | Right
  private type EmptyTile   = '.'
  private type Obstruction = '#'

  private type Tile = EmptyTile | Obstruction | Direction

  type Row  = Map[Int, Tile]
  type Grid = Map[Int, Row]

  private val directionToMovement: PartialFunction[Direction, Movement] =
    case _: Up    => (0, -1)
    case _: Down  => (0, 1)
    case _: Left  => (-1, 0)
    case _: Right => (1, 0)
  end directionToMovement

  private val rightTurn: PartialFunction[Movement, Movement] =
    case (0, -1) => (1, 0)
    case (1, 0)  => (0, 1)
    case (0, 1)  => (-1, 0)
    case (-1, 0) => (0, -1)
  end rightTurn

  private val parseInput: Seq[String] => (Grid, Position, Movement) = input =>
    input.zipWithIndex.foldLeft((Map.empty[Int, Row], (0, 0), (-1, -1))):
      case ((grid, position, direction), ((row, rowNumber))) =>
        val (colMap, start, dir) = row.zipWithIndex.foldLeft((Map.empty[Int, Tile], position, direction)):
          case ((colMap, start, dir), (col: Obstruction, colNumber)) => (colMap.updated(colNumber, col), start, dir)
          case ((colMap, start, dir), (col: EmptyTile, colNumber))   => (colMap.updated(colNumber, col), start, dir)
          case ((colMap, _, _), (col: Direction, colNumber))         => (colMap.updated(colNumber, col), (colNumber, rowNumber), directionToMovement(col))
          case (acc, _)                                              => acc
        (grid.updated(rowNumber, colMap), start, dir)
  end parseInput

  @tailrec
  private def walkGrid(grid: Grid, position: Position, move: Movement, history: Set[Position]): Set[Position] =
    val (x, y)         = position
    val (dx, dy)       = move
    val (nextX, nextY) = (x + dx, y + dy)
    grid.get(nextY).flatMap(_.get(nextX)) match
      case Some(_: Obstruction) => walkGrid(grid = grid, position = position, move = rightTurn(move), history = history + position)
      case Some(_)              => walkGrid(grid = grid, position = (nextX, nextY), move = move, history = history + position)
      case None                 => history + position
    end match
  end walkGrid

  @tailrec
  private def isGridLooping(grid: Grid, position: Position, move: Movement, history: Set[(Position, Movement)]): Boolean =
    val (x, y)         = position
    val (dx, dy)       = move
    val (nextX, nextY) = (x + dx, y + dy)
    grid.get(nextY).flatMap(_.get(nextX)) match
      case Some(_: Obstruction) => isGridLooping(grid = grid, position = position, move = rightTurn(move), history = history + ((position, move)))
      case Some(_) if history.contains((position, move)) => true
      case Some(_) => isGridLooping(grid = grid, position = (nextX, nextY), move = move, history = history + ((position, move)))
      case None    => false
    end match
  end isGridLooping

end DaySix
