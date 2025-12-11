package aoc2024.day04

import utils.DailyChallenge
import utils.syntax.*

import java.time.LocalDate
import scala.annotation.tailrec

object DayFour extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 4)

  override def partOne(input: Seq[String]): Int = findAllXmassesForPartOne(parseInput(input))

  override def partTwo(input: Seq[String]): Int = findAllMasCrossesForPartTwo(parseInput(input))

  @main def run(): Unit = evaluate()

  final case class Square(char: Char, x: Int, y: Int)
  private type Direction = (Int, Int)
  private type Grid      = Seq[Seq[Square]]

  val parseInput: Seq[String] => Grid =
    _.zipWithIndex.map((str, y) => str.toIndexedSeq.zipWithIndex.map((char, x) => Square(char = char, x = x, y = y)))

  private def possibleDirections(
      grid: Grid,
      square: Square,
      filterOpt: Option[Direction => Boolean] = None): Seq[Direction] = (
    for
      horizontal <- square.x match
                      case 0                                                => Seq(0, 1)
                      case x if grid.headOption.map(_.size).contains(x + 1) => Seq(-1, 0)
                      case _                                                => Seq(-1, 0, 1)
      vertical <- square.y match
                    case 0                       => Seq(0, 1)
                    case y if grid.size == y + 1 => Seq(-1, 0)
                    case _                       => Seq(-1, 0, 1)
    yield (horizontal, vertical)
  ).filter:
    case (0, 0)    => false
    case direction => filterOpt.forall(_(direction))

  @tailrec
  private def lookupWordInDirection(grid: Grid, square: Square, direction: Direction, word: String = "MAS"): Boolean =
    lazy val nextSquareOpt = nextSquareInLine(grid = grid, square = square, direction = direction)
    word.headOption match
      case Some(char) if word.length == 1 => nextSquareOpt.exists(_.char == char)
      case Some(char)                     => nextSquareOpt match
          case Some(nextSquare) if nextSquare.char == char =>
            lookupWordInDirection(grid = grid, square = nextSquare, direction = direction, word = word.tail)
          case _ => false
      case None => false
    end match
  end lookupWordInDirection

  private def nextSquareInLine(grid: Grid, square: Square, direction: Direction): Option[Square] =
    val (horizontalMovement, verticalMovement) = direction
    grid.lift(square.y + verticalMovement).flatMap(_.lift(square.x + horizontalMovement))
  end nextSquareInLine

  private val findAllXmassesForPartOne: Grid => Int = grid =>
    grid.foldLeft(0): (total, row) =>
      total + row.foldLeft(0):
        case (rowTotal, square) if square.char == 'X' =>
          rowTotal + possibleDirections(grid, square)
            .count(direction => lookupWordInDirection(grid = grid, square = square, direction = direction))
        case (rowTotal, _) => rowTotal
  end findAllXmassesForPartOne

  private def crossDirection(startingPoint: Square, direction: Direction, otherStartingPoint: Square): Direction =
    val (horizontal, vertical) = direction
    if startingPoint.x == otherStartingPoint.x then (horizontal, vertical * -1)
    else (horizontal * -1, vertical)
    end if
  end crossDirection

  private def otherStartPointsForCrossSearch(grid: Grid, square: Square, direction: Direction): Iterable[Square] =
    lazy val twoToTheRight = grid.lift(square.y).flatMap(_.lift(square.x + 2).filter(_.char == 'M'))
    lazy val twoDown       = grid.lift(square.y + 2).flatMap(_.lift(square.x).filter(_.char == 'M'))
    // only return relevant other starting points
    direction match
      case (1, 1)  => twoDown ++ twoToTheRight
      case (-1, 1) => twoDown
      case (1, -1) => twoToTheRight
      case _       => Nil
    end match
  end otherStartPointsForCrossSearch

  private val findAllMasCrossesForPartTwo: Grid => Int = grid =>
    val notEastWestNorthSouthOrNorthWest: Direction => Boolean =
      case (0, 1) | (1, 0) | (0, -1) | (-1, 0) | (-1, -1) => false
      case _                                              => true
    end notEastWestNorthSouthOrNorthWest

    grid.foldLeft(0): (total, row) =>
      total + row.foldLeft(0):
        case (rowTotal, square) if square.char == 'M' =>
          rowTotal + possibleDirections(grid, square, notEastWestNorthSouthOrNorthWest.some).count: direction =>
            if lookupWordInDirection(grid = grid, square = square, direction = direction, word = "AS")
            then
              otherStartPointsForCrossSearch(grid, square, direction)
                .exists: otherStartingPoint =>
                  lookupWordInDirection(
                    grid = grid,
                    square = otherStartingPoint,
                    direction = crossDirection(startingPoint = square,
                                               direction = direction,
                                               otherStartingPoint = otherStartingPoint,
                    ),
                    word = "AS",
                  )
            else false
        case (rowTotal, _) => rowTotal
  end findAllMasCrossesForPartTwo

end DayFour
